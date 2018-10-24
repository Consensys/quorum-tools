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
	"path/filepath"
	"time"

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
)

type Quorum struct {
	*DefaultConfigurable

	containerName string
	containerId   string
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
		},
		&container.HostConfig{
			Binds: []string{
				fmt.Sprintf("%s:%s", q.DataDir().Base, defaultQuorumContainerWorkingDir),
				fmt.Sprintf("%s:%s", q.TxManager().DataDir(), defaultTxManagerContainerWorkingDir),
			},
			PortBindings: map[nat.Port][]nat.PortBinding{
				nat.Port(fmt.Sprintf("%d/tcp", node.DefaultHTTPPort)): {
					nat.PortBinding{
						HostIP: "0.0.0.0",
						HostPort: fmt.Sprintf("%d", defaultQuorumRPCInitPort+q.Index()),
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
	containerId := resp.ID
	shortContainerId := containerId[:6]
	if err := q.DockerClient().ContainerStart(context.Background(), containerId, types.ContainerStartOptions{}); err != nil {
		return fmt.Errorf("start: can't start container %s - %s", shortContainerId, err)
	}

	healthyContainer := &helper.StateChangeConfig{
		Target:       []string{"healthy"},
		PollInterval: 3 * time.Second,
		Timeout:      60 * time.Second,
		Refresh: func() (*helper.StateResult, error) {
			c, err := q.DockerClient().ContainerInspect(context.Background(), containerId)
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

	q.containerId = containerId
	return nil
}

func (q *Quorum) Stop() error {
	duration := 30 * time.Second
	return q.DockerClient().ContainerStop(context.Background(), q.containerId, &duration)
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
	args := make([]string, 0)
	for k, v := range combinedConfig {
		if len(v) == 0 {
			args = append(args, k)
		} else {
			args = append(args, []string{k, v}...)
		}
	}
	return args
}

func hostnameQuorum(idx int) string {
	return fmt.Sprintf("node%d", idx)
}
