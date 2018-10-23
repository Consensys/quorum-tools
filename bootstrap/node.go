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

package bootstrap

import (
	"crypto/ecdsa"

	"github.com/ethereum/go-ethereum/p2p/discover"
)

type Node struct {
	P2PPort        int
	IP             string
	DefaultAccount *Account
	NodeKey        *ecdsa.PrivateKey
	Enode          string
	DataDir        *DataDir
}

func NewNode(tmpDir string, ip string, port int) (*Node, error) {
	datadir, err := NewDataDir(tmpDir)
	if err != nil {
		return nil, err
	}
	acc, err := NewAccount(datadir.KeystoreDir)
	if err != nil {
		return nil, err
	}
	nodeKey, err := NewNodeKey()
	if err != nil {
		return nil, err
	}
	return &Node{
		Enode:          discover.PubkeyID(&nodeKey.PublicKey).String(),
		P2PPort:        port,
		IP:             ip,
		DataDir:        datadir,
		DefaultAccount: acc,
		NodeKey:        nodeKey,
	}, nil
}
