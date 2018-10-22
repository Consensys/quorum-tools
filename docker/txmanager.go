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
	"io/ioutil"
	"os"
	"path/filepath"
	"time"

	"github.com/ethereum/go-ethereum/log"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/container"
)

type TxManager interface {
	GenerateKeys() ([]byte, []byte, error)
}

type TesseraTxManager struct {
	*DefaultConfigurable
}

func (t *TesseraTxManager) Start() error {
	return nil
}

func (t *TesseraTxManager) Stop() error {
	return nil
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
	log.Debug("Start Container", "id", containerId)
	if err := t.DockerClient().ContainerStart(context.Background(), containerId, types.ContainerStartOptions{}); err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: can't start container %s - %s", containerId, err)
	}
	defer t.DockerClient().ContainerRemove(context.Background(), containerId, types.ContainerRemoveOptions{Force: true})
	// Attach container: for stdin interaction with the container.
	attachResp, err := t.DockerClient().ContainerAttach(context.Background(), containerId, types.ContainerAttachOptions{Stream: true, Stdin: true, Stderr: true})
	if err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: failed to attach to container %s - %s", containerId, err)
	}
	// - write empty string password to container stdin
	attachResp.Conn.Write([]byte{10, 13, 10, 13}) //Empty password

	timeoutCtx, cancelCtx := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancelCtx()
	log.Debug("Wait for container to be up", "id", containerId)
	if _, err := t.DockerClient().ContainerWait(timeoutCtx, containerId); err != nil {
		return nil, nil, fmt.Errorf("GenerateKeys: container %s is not running - %s", containerId, err)
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
		&DefaultConfigurable{
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
