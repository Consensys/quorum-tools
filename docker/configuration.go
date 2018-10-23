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
	"github.com/docker/docker/client"
)

const (
	CfgKeyDockerClient         = "DockerClient"
	CfgKeyDockerImage          = "DockerImage"
	CfgKeyDockerNetwork        = "DockerNetwork"
	CfgKeyMyIP                 = "MyIP"
	CfgKeyConfig               = "Config"
	CfgKeyNodeIndex            = "NodeIndex"
	CfgKeyTxManagerPublicKeys  = "TxManagerPublicKeys"
	CfgKeyTxManagerPrivateKeys = "TxManagerPrivateKeys"
	CfgKeyLabels               = "Labels"
	CfgKeyProvisionId          = "ProvisionId"
	CfgKeyNodeCount            = "NodeCount"
)

type ConfigureFn func(c Configurable)

type Configurable interface {
	Set(key string, value interface{})
}

type DefaultConfigurable struct {
	configuration map[string]interface{}
}

func (dc *DefaultConfigurable) Set(key string, value interface{}) {
	dc.configuration[key] = value
}

func (dc *DefaultConfigurable) DockerClient() *client.Client {
	return dc.configuration[CfgKeyDockerClient].(*client.Client)
}

func (dc *DefaultConfigurable) DockerImage() string {
	return dc.configuration[CfgKeyDockerImage].(string)
}

func (dc *DefaultConfigurable) DockerNetwork() *Network {
	return dc.configuration[CfgKeyDockerNetwork].(*Network)
}

func (dc *DefaultConfigurable) Labels() map[string]string {
	return dc.configuration[CfgKeyLabels].(map[string]string)
}

func (dc *DefaultConfigurable) Index() int {
	return dc.configuration[CfgKeyNodeIndex].(int)
}

func (dc *DefaultConfigurable) ProvisionId() string {
	return dc.configuration[CfgKeyProvisionId].(string)
}

func (dc *DefaultConfigurable) TxManagerPublicKeys() [][]byte {
	return dc.configuration[CfgKeyTxManagerPublicKeys].([][]byte)
}

func (dc *DefaultConfigurable) TxManagerPrivateKeys() [][]byte {
	return dc.configuration[CfgKeyTxManagerPrivateKeys].([][]byte)
}

func (dc *DefaultConfigurable) MyIP() string {
	return dc.configuration[CfgKeyMyIP].(string)
}

func (dc *DefaultConfigurable) NodeCount() int {
	return dc.configuration[CfgKeyNodeCount].(int)
}

func (dc *DefaultConfigurable) Config() map[string]string {
	return dc.configuration[CfgKeyConfig].(map[string]string)
}

func ConfigureNodeIndex(idx int) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyNodeIndex, idx)
	}
}

func ConfigureDockerClient(client *client.Client) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyDockerClient, client)
	}
}

func ConfigureDockerImage(image string) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyDockerImage, image)
	}
}

func ConfigureNetwork(n *Network) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyDockerNetwork, n)
	}
}

func ConfigureConfig(cfg map[string]string) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyConfig, cfg)
	}
}

func ConfigureLabels(labels map[string]string) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyLabels, labels)
	}
}

func ConfigureProvisionId(id string) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyProvisionId, id)
	}
}

func ConfigureMyIP(ip string) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyMyIP, ip)
	}
}

func ConfigureNodeCount(n int) ConfigureFn {
	return func(c Configurable) {
		c.Set(CfgKeyNodeCount, n)
	}
}
