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
	"bytes"
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"text/template"
	"time"

	"github.com/docker/docker/api/types/network"

	"github.com/ethereum/go-ethereum/log"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
)

const defaultTesseraPort = 9000

type TxManager interface {
	GenerateKeys() ([]byte, []byte, error)
}

type TesseraTxManager struct {
	*DefaultConfigurable

	containerId string
	hostDataDir string
}

func (t *TesseraTxManager) Start() error {
	tmpDataDir, err := ioutil.TempDir("", fmt.Sprintf("qctl-%d", time.Now().Unix()))
	if err != nil {
		return fmt.Errorf("start: can't create tmp dir - %s", err)
	}
	tmpDataDir, _ = filepath.EvalSymlinks(tmpDataDir)
	log.Debug("Create temp directory", "path", tmpDataDir)
	containerWorkingDir := "/tm"
	configFileName := "config.json"
	// prepare config file
	tmpl := template.Must(template.New("tesseraConfig").Parse(tesseraConfigTemplate))
	tmplData := tesseraTemplate{
		DataDir:   containerWorkingDir,
		IP:        t.MyIP(),
		Port:      defaultTesseraPort,
		Keys:      make([]tesseraTemplateKey, len(t.TxManagerPrivateKeys())),
		PeerNames: make([]string, t.NodeCount()),
	}
	for i, k := range t.TxManagerPrivateKeys() {
		tmplData.Keys[i].Private = string(k)
	}
	for i, k := range t.TxManagerPublicKeys() {
		tmplData.Keys[i].Public = string(k)
	}
	for i := 1; i < t.NodeCount(); i++ {
		tmplData.PeerNames[i] = hostname(i)
	}
	var jsonData bytes.Buffer
	if err := tmpl.Execute(&jsonData, tmplData); err != nil {
		return fmt.Errorf("start: can't create config - %s", err)
	}
	log.Debug("Write Tessera Config", "content", string(jsonData.Bytes()))
	if err := ioutil.WriteFile(filepath.Join(tmpDataDir, configFileName), jsonData.Bytes(), 0644); err != nil {
		return err
	}
	resp, err := t.DockerClient().ContainerCreate(
		context.Background(),
		&container.Config{
			Image:      t.DockerImage(),
			WorkingDir: containerWorkingDir,
			Cmd: []string{
				"-configfile",
				configFileName,
			},
			Labels:   t.Labels(),
			Hostname: hostname(t.Index()),
			Healthcheck: &container.HealthConfig{
				Interval:    3 * time.Second,
				Retries:     5,
				StartPeriod: 5 * time.Second,
				Timeout:     10 * time.Second,
				Test: []string{
					"CMD",
					"wget", "--spider", fmt.Sprintf("http://localhost:%d/upcheck", defaultTesseraPort),
				},
			},
		},
		&container.HostConfig{
			Binds: []string{
				fmt.Sprintf("%s:%s", tmpDataDir, containerWorkingDir),
			},
		},
		&network.NetworkingConfig{
			EndpointsConfig: map[string]*network.EndpointSettings{
				t.DockerNetwork().name: {
					NetworkID: t.DockerNetwork().id,
					IPAMConfig: &network.EndpointIPAMConfig{
						IPv4Address: t.MyIP(),
					},
				},
			},
		},
		fmt.Sprintf("%s_TxManager_%d", t.ProvisionId(), t.Index()),
	)
	if err != nil {
		return fmt.Errorf("start: can't create container - %s", err)
	}
	containerId := resp.ID
	shortContainerId := containerId[:6]
	if err := t.DockerClient().ContainerStart(context.Background(), containerId, types.ContainerStartOptions{}); err != nil {
		return fmt.Errorf("start: can't start container %s - %s", shortContainerId, err)
	}

	t.containerId = containerId
	t.hostDataDir = tmpDataDir
	return nil
}

func (t *TesseraTxManager) Stop() error {
	duration := 30 * time.Second
	return t.DockerClient().ContainerStop(context.Background(), t.containerId, &duration)
}

func (t *TesseraTxManager) GenerateKeys() (public []byte, private []byte, retErr error) {
	tmpDataDir, err := ioutil.TempDir("", fmt.Sprintf("qctl-%d", time.Now().Unix()))
	if err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: can't create tmp dir - %s", err)
	}
	tmpDataDir, _ = filepath.EvalSymlinks(tmpDataDir)
	log.Debug("Create temp directory", "path", tmpDataDir)
	defer os.RemoveAll(tmpDataDir)
	containerWorkingDir := "/tm"
	resp, err := t.DockerClient().ContainerCreate(
		context.Background(),
		&container.Config{
			Image:      t.DockerImage(),
			WorkingDir: containerWorkingDir,
			Cmd: []string{
				"-keygen",
			},
			Labels:    t.Labels(),
			Tty:       true,
			OpenStdin: true,
		},
		&container.HostConfig{
			Binds: []string{
				fmt.Sprintf("%s:%s", tmpDataDir, containerWorkingDir),
			},
		},
		nil,
		fmt.Sprintf("%s_TxManager_KeyGen_%d", t.ProvisionId(), t.Index()),
	)
	if err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: can't create container - %s", err)
	}
	containerId := resp.ID
	shortContainerId := containerId[:6]
	log.Debug("Start Tessera Container to generate keys", "id", shortContainerId)
	if err := t.DockerClient().ContainerStart(context.Background(), containerId, types.ContainerStartOptions{}); err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: can't start container %s - %s", shortContainerId, err)
	}
	defer t.DockerClient().ContainerRemove(context.Background(), containerId, types.ContainerRemoveOptions{Force: true})
	// Attach container: for stdin interaction with the container.
	attachResp, err := t.DockerClient().ContainerAttach(context.Background(), containerId, types.ContainerAttachOptions{Stream: true, Stdin: true, Stderr: true})
	if err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: failed to attach to container %s - %s", shortContainerId, err)
	}
	// - write empty string password to container stdin
	attachResp.Conn.Write([]byte{10, 13, 10, 13}) //Empty password

	timeoutCtx, cancelCtx := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancelCtx()
	log.Debug("Wait for container to be up", "id", shortContainerId)
	if _, err := t.DockerClient().ContainerWait(timeoutCtx, containerId); err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: container %s is not running - %s", shortContainerId, err)
	}

	// now read key files generated by the above run
	public, retErr = ioutil.ReadFile(filepath.Join(tmpDataDir, ".pub"))
	if retErr != nil {
		return nil, nil, retErr
	}
	private, retErr = ioutil.ReadFile(filepath.Join(tmpDataDir, ".key"))
	return
}

func NewTesseraTxManager(configureFns ...ConfigureFn) (Container, error) {
	tm := &TesseraTxManager{
		DefaultConfigurable: &DefaultConfigurable{
			configuration: make(map[string]interface{}),
		},
	}
	for _, cfgFn := range configureFns {
		cfgFn(tm)
	}
	public, private, err := tm.GenerateKeys()
	if err != nil {
		return nil, err
	}
	tm.Set(CfgKeyTxManagerPublicKeys, [][]byte{public})
	tm.Set(CfgKeyTxManagerPrivateKeys, [][]byte{private})
	return tm, nil
}

type tesseraTemplateKey struct {
	Public  string
	Private string
}

type tesseraTemplate struct {
	DataDir   string
	IP        string
	Port      int
	PeerNames []string
	Keys      []tesseraTemplateKey
}

const tesseraConfigTemplate = `
	  {
          "useWhiteList": false,
          "jdbc": {
              "username": "sa",
              "password": "",
              "url": "jdbc:h2:./{{.DataDir}}/db;MODE=Oracle;TRACE_LEVEL_SYSTEM_OUT=0"
          },
          "server": {
              "port": {{.Port}},
              "hostName": "http://{{.IP}}",
              "sslConfig": {
                  "tls": "OFF",
                  "generateKeyStoreIfNotExisted": true,
                  "serverKeyStore": "{{.DataDir}}/server-keystore",
                  "serverKeyStorePassword": "quorum",
                  "serverTrustStore": "{{.DataDir}}/server-truststore",
                  "serverTrustStorePassword": "quorum",
                  "serverTrustMode": "TOFU",
                  "knownClientsFile": "{{.DataDir}}/knownClients",
                  "clientKeyStore": "{{.DataDir}}/client-keystore",
                  "clientKeyStorePassword": "quorum",
                  "clientTrustStore": "{{.DataDir}}/client-truststore",
                  "clientTrustStorePassword": "quorum",
                  "clientTrustMode": "TOFU",
                  "knownServersFile": "{{.DataDir}}/knownServers"
              }
          },
          "peer": [
              {
                  "url": "http://{{.IP}}:{{.Port}}"
              }
			{{ range $index, $name := .PeerNames }}
			  ,{
                  "url": "http://{{$name}}:{{$.Port}}"
              }
			{{ end }}
          ],
          "keys": {
              "passwords": [],
              "keyData": [
				{{ range $i, $k := .Keys }}
					{{ if $i }}, {{ end }}
                  {
                      "config": {{ $k.Private }},
                      "publicKey": "{{ $k.Public }}"
                  }	
				{{ end }}
              ]
          },
          "alwaysSendTo": [],
          "unixSocketFile": "{{.DataDir}}/tm.ipc"
      }
`

func hostname(idx int) string {
	return fmt.Sprintf("txmanager%d", idx+1)
}
