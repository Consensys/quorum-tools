/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package docker

import (
	"context"
	"fmt"
	"io"
	"path/filepath"
	"strconv"
	"text/template"
	"time"

	"github.com/docker/docker/client"

	"github.com/jpmorganchase/quorum-tools/bootstrap"

	"github.com/docker/go-connections/nat"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
	"github.com/docker/docker/api/types/network"
	"github.com/jpmorganchase/quorum-tools/helper"

	"github.com/ethereum/go-ethereum/core"
	"github.com/ethereum/go-ethereum/node"

	"github.com/ethereum/go-ethereum/log"
)

const (
	defaultQuorumRPCInitPort         = 22000
	defaultQuorumContainerWorkingDir = "/qdata"
	defaultRaftPort                  = 50400
)

type QuorumNetwork struct {
	NodeCount    int // total number of nodes including stopped/killed nodes
	TxManagers   []TxManager
	QuorumNodes  []*Quorum
	Genesis      *core.Genesis
	dockerClient *client.Client
}

type Quorum struct {
	*DefaultConfigurable

	containerName string
	containerId   string
	rpcPort       int
}

func NewQuorum(configureFns ...ConfigureFn) (Container, error) {
	q := &Quorum{
		DefaultConfigurable: &DefaultConfigurable{
			configuration: make(map[string]interface{}),
		},
	}
	for _, cfgFn := range configureFns {
		cfgFn(q)
	}
	// init datadir
	config := &node.DefaultConfig
	config.DataDir = q.DataDir().Base
	config.Name = filepath.Base(q.DataDir().GethDir)
	stack, err := node.New(config)
	if err != nil {
		return nil, err
	}
	for _, name := range []string{"chaindata", "lightchaindata"} {
		chaindb, err := stack.OpenDatabase(name, 0, 0)
		if err != nil {
			return nil, err
		}
		_, hash, err := core.SetupGenesisBlock(chaindb, q.Genesis())
		if err != nil {
			return nil, err
		}
		log.Info("Successfully wrote genesis state", "node", q.Index(), "database", name, "hash", hash)
	}
	q.containerName = fmt.Sprintf("%s_Node_%d", q.ProvisionId(), q.Index())
	return q, nil
}

func (q *Quorum) Name() string {
	return q.containerName
}

func (q *Quorum) Start() error {
	q.rpcPort = defaultQuorumRPCInitPort + q.Index()
	additionalExposedPorts := make(map[nat.Port]struct{})
	if q.ConsensusAlgorithm() == "raft" {
		additionalExposedPorts[nat.Port(fmt.Sprintf("%d", defaultRaftPort))] = struct{}{}
	}
	resp, err := q.DockerClient().ContainerCreate(
		context.Background(),
		&container.Config{
			Image:      q.DockerImage(),
			WorkingDir: defaultQuorumContainerWorkingDir,
			Cmd:        q.makeArgs(),
			Labels:     q.Labels(),
			Hostname:   hostnameQuorum(q.Index()),
			Healthcheck: &container.HealthConfig{
				Interval:    3 * time.Second,
				Retries:     5,
				StartPeriod: 5 * time.Second,
				Timeout:     10 * time.Second,
				Test: []string{
					"CMD",
					"wget", "--spider", fmt.Sprintf("http://localhost:%d", node.DefaultHTTPPort),
				},
			},
			Env: []string{
				fmt.Sprintf("PRIVATE_CONFIG=%s", q.TxManager().SocketFile()),
			},
			ExposedPorts: additionalExposedPorts,
		},
		&container.HostConfig{
			Binds: []string{
				fmt.Sprintf("%s:%s", q.DataDir().Base, defaultQuorumContainerWorkingDir),
				fmt.Sprintf("%s:%s", q.TxManager().DataDir(), defaultTxManagerContainerWorkingDir),
			},
			PortBindings: map[nat.Port][]nat.PortBinding{
				nat.Port(fmt.Sprintf("%d/tcp", node.DefaultHTTPPort)): {
					nat.PortBinding{
						HostIP:   "0.0.0.0",
						HostPort: fmt.Sprintf("%d", q.rpcPort),
					},
				},
			},
		},
		&network.NetworkingConfig{
			EndpointsConfig: map[string]*network.EndpointSettings{
				q.DockerNetwork().name: {
					NetworkID: q.DockerNetwork().id,
					IPAMConfig: &network.EndpointIPAMConfig{
						IPv4Address: q.MyIP(),
					},
					Aliases: []string{
						hostnameQuorum(q.Index()),
					},
				},
			},
		},
		q.containerName,
	)
	if err != nil {
		return fmt.Errorf("start: can't create container - %s", err)
	}
	q.containerId = resp.ID
	return q.SoftStart()
}

func (q *Quorum) Stop() error {
	duration := 30 * time.Second
	return q.DockerClient().ContainerStop(context.Background(), q.containerId, &duration)
}

func (q *Quorum) Url() string {
	return fmt.Sprintf("http://%s:%d", q.MyIP(), q.rpcPort)
}

func (q *Quorum) makeArgs() []string {
	combinedConfig := make(map[string]string)
	// first construct our config
	combinedConfig["--datadir"] = defaultQuorumContainerWorkingDir
	combinedConfig["--rpc"] = ""
	combinedConfig["--rpcaddr"] = "0.0.0.0"
	combinedConfig["--rpcapi"] = fmt.Sprintf("admin,db,eth,debug,miner,net,shh,txpool,personal,web3,quorum,%s", q.ConsensusAlgorithm())
	combinedConfig["--unlock"] = "0"
	combinedConfig["--password"] = filepath.Join(defaultQuorumContainerWorkingDir, "passwords.txt")
	combinedConfig["--nodiscover"] = ""
	combinedConfig["--networkid"] = "2018"
	combinedConfig["--identity"] = hostnameQuorum(q.Index())
	// combinedConfig["--ipcdisable"] = ""
	combinedConfig["--permissioned"] = ""
	// now override with config from Node
	for k, v := range q.Config() {
		combinedConfig[fmt.Sprintf("--%s", k)] = v
	}
	// then add consensus config
	for k, v := range q.ConsensusGethArgs() {
		combinedConfig[k] = v
	}
	// don't allow to override some fundamental configs like raft
	if q.ConsensusAlgorithm() == "raft" {
		combinedConfig["--raft"] = ""
		combinedConfig["--raftport"] = fmt.Sprintf("%d", defaultRaftPort)
	}
	args := make([]string, 0)
	for k, v := range combinedConfig {
		if len(k) == 0 {
			continue
		}
		if len(v) == 0 {
			args = append(args, k)
		} else {
			args = append(args, []string{k, v}...)
		}
	}
	return args
}

// start an existing container
func (q *Quorum) SoftStart() error {
	shortContainerId := q.containerId[:6]
	if err := q.DockerClient().ContainerStart(context.Background(), q.containerId, types.ContainerStartOptions{}); err != nil {
		return fmt.Errorf("start: can't start container %s - %s", shortContainerId, err)
	}

	healthyContainer := &helper.StateChangeConfig{
		Target:       []string{"healthy"},
		PollInterval: 3 * time.Second,
		Timeout:      60 * time.Second,
		Refresh: func() (*helper.StateResult, error) {
			c, err := q.DockerClient().ContainerInspect(context.Background(), q.containerId)
			if err != nil {
				return nil, err
			}
			return &helper.StateResult{
				Result: c,
				State:  c.State.Health.Status,
			}, nil
		},
	}

	if _, err := healthyContainer.Wait(); err != nil {
		return err
	}
	return nil
}

func (qn *QuorumNetwork) AddNodes(newNodes []QuorumBuilderNode) (newIds []int, retErr error) {
	newIds = make([]int, len(newNodes))
	for i := 0; i < len(newIds); i++ {
		newIds[i] = i + qn.NodeCount
		qn.NodeCount++
	}
	newLabels := make(map[string]string)
	for k, v := range CurrrentBuilder.commonLabels {
		newLabels[k] = v
	}
	newLabels["com.quorum.operator.createdAt"] = time.Now().Format(time.RFC1123Z)
	newBuilder := &QuorumBuilder{
		Consensus:     CurrrentBuilder.Consensus,
		Name:          CurrrentBuilder.Name,
		Genesis:       CurrrentBuilder.Genesis,
		tmpDir:        CurrrentBuilder.tmpDir,
		dockerNetwork: CurrrentBuilder.dockerNetwork,
		pullMux:       CurrrentBuilder.pullMux,
		dockerClient:  CurrrentBuilder.dockerClient,
		commonLabels:  newLabels,
		Nodes:         newNodes,
	}
	defer func() {
		// clean up containers if there's anything happen
		// as we already added additional tags so it only clean up ones that's created
		if retErr != nil {
			newBuilder.Destroy(false)
		}
	}()
	// start tx
	if err := newBuilder.startContainers(func(idx int, node QuorumBuilderNode) (Container, error) {
		ips, err := newBuilder.dockerNetwork.GetFreeIPAddrs(1)
		if err != nil {
			return nil, err
		}
		txManager, err := newBuilder.prepareTxManager(newIds[idx], qn.NodeCount, node, ips[0].String())
		if err != nil {
			return nil, err
		}
		qn.TxManagers = append(qn.TxManagers, txManager.(TxManager))
		return txManager, nil
	}); err != nil {
		retErr = err
		return
	}
	bsNodes := make([]*bootstrap.Node, qn.NodeCount)
	for i := 0; i < qn.NodeCount; i++ {
		if i >= newIds[0] { // this is for new nodes
			port, err := strconv.Atoi(node.DefaultConfig.P2P.ListenAddr[1:])
			if err != nil {
				retErr = err
				return
			}
			ips, err := newBuilder.dockerNetwork.GetFreeIPAddrs(1)
			if err != nil {
				retErr = err
				return
			}
			bsNodes[i], err = bootstrap.NewNode(newBuilder.tmpDir, i, ips[0].String(), port)
			if err != nil {
				retErr = err
				return
			}
		} else {
			existingQuorumNode := qn.QuorumNodes[i]
			bsNodes[i] = &bootstrap.Node{
				IP:             existingQuorumNode.MyIP(),
				NodeKey:        existingQuorumNode.NodeKey(),
				DataDir:        existingQuorumNode.DataDir(),
				DefaultAccount: existingQuorumNode.DefaultAccount(),
				Enode:          existingQuorumNode.BootstrapData().Enode,
				P2PPort:        existingQuorumNode.BootstrapData().P2PPort,
			}
		}
	}
	// Update permissioned-nodes and static-nodes files in current one
	if err := bootstrap.WritePermissionedNodes(bsNodes, defaultRaftPort); err != nil {
		retErr = err
		return
	}
	// start Quorum
	if err := newBuilder.startContainers(func(idx int, meta QuorumBuilderNode) (Container, error) {
		q, err := newBuilder.prepareQuorum(newIds[idx], qn.NodeCount, meta, bsNodes[newIds[idx]], qn.TxManagers[newIds[idx]], qn.Genesis)
		if err != nil {
			return nil, err
		}
		qn.QuorumNodes = append(qn.QuorumNodes, q.(*Quorum))
		return q, nil
	}); err != nil {
		retErr = err
		return
	}
	return
}

func (qn *QuorumNetwork) WriteNetworkConfigurationYAML(file io.Writer) error {
	tmpl := template.Must(template.New("networkConfiguration").Funcs(template.FuncMap{
		"inc": func(i int) int {
			return i + 1
		},
	}).Parse(`
quorum:
  nodes:
    {{- range $index, $data := .Nodes }}
    Node{{- inc $index }}:
      privacy-address: {{- $data.PrivacyAddress }}
      url: {{- $data.Url }}
    {{- end }}
`))
	tmplData := make([]map[string]string, len(qn.QuorumNodes))
	for i := 0; i < len(qn.QuorumNodes); i++ {
		tmplData[i] = make(map[string]string)
		tmplData[i]["PrivacyAddress"] = fmt.Sprintf(" %s", qn.TxManagers[i].Address())
		tmplData[i]["Url"] = fmt.Sprintf(" http://localhost:%d", qn.QuorumNodes[i].rpcPort)
	}
	if err := tmpl.Execute(file, struct {
		Nodes []map[string]string
	}{
		Nodes: tmplData,
	}); err != nil {
		return err
	}
	return nil
}

func (qn *QuorumNetwork) PerformAction(idx int, target string, action string) error {
	if action == "stop" && target == "quorum" {
		return qn.QuorumNodes[idx].Stop()
	}
	if action == "stop" && target == "tx_manager" {
		return qn.TxManagers[idx].(Container).Stop()
	}
	if action == "start" && target == "quorum" {
		return qn.QuorumNodes[idx].SoftStart()
	}
	if action == "start" && target == "tx_manager" {
		return qn.TxManagers[idx].(Container).SoftStart()
	}
	if action == "restart" && target == "quorum" {
		if err := qn.QuorumNodes[idx].Stop(); err != nil {
			return err
		}
		return qn.QuorumNodes[idx].SoftStart()
	}
	if action == "restart" && target == "tx_manager" {
		if err := qn.TxManagers[idx].(Container).Stop(); err != nil {
			return err
		}
		return qn.TxManagers[idx].(Container).SoftStart()
	}
	return fmt.Errorf("not implemented")
}
func (qn *QuorumNetwork) StreamLogs(idx int, target string, responseChan chan []byte) error {
	defer func() {
		log.Debug("Stop log streaming", "target", target, "idx", idx)
	}()
	log.Debug("Start log streaming", "target", target, "idx", idx)
	containerId := qn.QuorumNodes[idx].containerId
	if target == "tx_manager" {
		containerId = qn.TxManagers[idx].ContainerId()
	}

	resp, err := qn.dockerClient.ContainerLogs(context.Background(), containerId, types.ContainerLogsOptions{
		ShowStderr: true,
		ShowStdout: true,
		Follow:     true,
	})
	if err != nil {
		return err
	}
	if _, err := io.Copy(&helper.ChanWriter{Chan: responseChan}, resp); err != nil {
		return err
	}
	return nil
}

func hostnameQuorum(idx int) string {
	return fmt.Sprintf("node%d", idx)
}
