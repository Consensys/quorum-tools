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
	"io"
	"io/ioutil"

	"github.com/docker/docker/client"

	"gopkg.in/yaml.v2"
)

type QuorumBuilderConsensus struct {
	Name   string            `yaml:"name"`
	Config map[string]string `yaml:"config"`
}

type QuorumBuilderNodeDocker struct {
	Image  string            `yaml:"image"`
	Config map[string]string `yaml:"config"`
}

type QuorumBuilderNode struct {
	Quorum    QuorumBuilderNodeDocker `yaml:"quorum"`
	TxManager QuorumBuilderNodeDocker
}

type QuorumBuilder struct {
	Name      string                 `yaml:"name"`
	Genesis   string                 `yaml:"genesis"`
	Consensus QuorumBuilderConsensus `yaml:"consensus"`
	Nodes     []QuorumBuilderNode    `yaml:"nodes"`

	dockerClient  *client.Client
	dockerNetwork *Network
	txManager     *TxManager
}

func NewQuorumBuilder(r io.Reader) (*QuorumBuilder, error) {
	b := &QuorumBuilder{}
	data, err := ioutil.ReadAll(r)
	if err != nil {
		return nil, err
	}
	if err := yaml.Unmarshal(data, b); err != nil {
		return nil, err
	}
	b.dockerClient, err = client.NewClientWithOpts(client.FromEnv)
	if err != nil {
		return nil, err
	}
	return b, nil
}

// 1. Build Docker Network
// 2. Build Tx Manager
// 3. Build Quorum
func (q *QuorumBuilder) Build() error {
	if err := q.buildDockerNetwork(); err != nil {
		return err
	}
	if err := q.buildTxManager(); err != nil {
		return err
	}
	return nil
}

func (q *QuorumBuilder) buildTxManager() error {
	tm, err := NewTesseraTxManager()
	if err != nil {
		return err
	}
	q.txManager = tm
	return nil
}

func (q *QuorumBuilder) buildDockerNetwork() error {
	network, err := NewDockerNetwork(q.dockerClient, q.Name)
	if err != nil {
		return err
	}
	q.dockerNetwork = network
	return nil
}
