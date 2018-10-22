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
	"errors"
	"fmt"
	"net"
	"sync"

	"github.com/docker/docker/api/types"
	"github.com/docker/docker/api/types/network"
	"github.com/docker/docker/client"
)

const (
	FirstOctet  = 172
	SecondOctet = 17
)

type NetworkManager interface {
	TryGetFreeSubnet() string
}

var defaultNetworkManager = newNetworkManager()

func newNetworkManager() *networkManager {
	return &networkManager{
		secondOctet: SecondOctet,
	}
}

type networkManager struct {
	mutex       sync.Mutex
	secondOctet int
}

func (n *networkManager) TryGetFreeSubnet() string {
	n.mutex.Lock()
	defer n.mutex.Unlock()
	n.secondOctet++
	return fmt.Sprintf("%d.%d.0.0/16", FirstOctet, n.secondOctet)
}

type Network struct {
	client  *client.Client
	id      string
	name    string
	ipv4Net *net.IPNet

	mutex   sync.Mutex
	ipIndex net.IP
}

func NewDockerNetwork(c *client.Client, networkName string, labels map[string]string) (*Network, error) {
	network := &Network{
		client: c,
	}

	if err := network.create(networkName, labels); err != nil {
		return nil, err
	}

	return network, nil
}

// create creates a user-defined docker network
func (n *Network) create(networkName string, labels map[string]string) error {
	n.name = networkName

	var maxTryCount = 15
	var err error
	var cResp types.NetworkCreateResponse
	var subnet string
	for i := 0; i < maxTryCount && n.id == ""; i++ {
		subnet = defaultNetworkManager.TryGetFreeSubnet()
		ipam := &network.IPAM{
			Config: []network.IPAMConfig{
				{
					Subnet: subnet,
				},
			},
		}
		cResp, err = n.client.NetworkCreate(context.Background(), n.name, types.NetworkCreate{
			IPAM:   ipam,
			Labels: labels,
		})
		if err == nil {
			break
		}
	}

	if err != nil {
		return err
	}
	n.id = cResp.ID
	_, n.ipv4Net, err = net.ParseCIDR(subnet)
	if err != nil {
		return err
	}
	// IP starts with xxx.xxx.0.1
	// Because xxx.xxx.0.1 is reserved for default Gateway IP
	n.ipIndex = net.IPv4(n.ipv4Net.IP[0], n.ipv4Net.IP[1], 0, 1)
	return nil
}

func (n *Network) ID() string {
	return n.id
}

func (n *Network) Name() string {
	return n.name
}

func (n *Network) Subnet() string {
	return n.ipv4Net.String()
}

func (n *Network) Remove() error {
	return n.client.NetworkRemove(context.Background(), n.id)
}

func (n *Network) GetFreeIPAddrs(num int) ([]net.IP, error) {
	n.mutex.Lock()
	defer n.mutex.Unlock()

	ips := make([]net.IP, 0)
	for len(ips) < num && n.ipv4Net.Contains(n.ipIndex) {
		ip := dupIP(n.ipIndex)
		for j := len(ip) - 1; j >= 0; j-- {
			ip[j]++
			if ip[j] > 0 {
				break
			}
		}
		n.ipIndex = ip
		ips = append(ips, ip)
	}

	if len(ips) != num {
		return nil, errors.New("insufficient IP addresses")
	}
	return ips, nil
}

func dupIP(ip net.IP) net.IP {
	// To save space, try and only use 4 bytes
	if x := ip.To4(); x != nil {
		ip = x
	}
	dup := make(net.IP, len(ip))
	copy(dup, ip)
	return dup
}
