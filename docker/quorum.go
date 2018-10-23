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
	"path/filepath"

	"github.com/ethereum/go-ethereum/core"
	"github.com/ethereum/go-ethereum/node"

	"github.com/ethereum/go-ethereum/log"
)

const (
	defaultQuorumP2PPort = 22000
)

type Quorum struct {
	*DefaultConfigurable
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
	return q, nil
}

func (q *Quorum) Start() error {
	return nil
}

func (q *Quorum) Stop() error {
	return nil
}
