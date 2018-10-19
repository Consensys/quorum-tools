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

import "github.com/docker/docker/client"

const (
	CfgKeyDockerClient  = "DockerClient"
	CfgKeyDockerImage   = "DockerImage"
	CfgKeyDockerNetwork = "DockerNetwork"
	CfgKeyConfig        = "Config"
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
