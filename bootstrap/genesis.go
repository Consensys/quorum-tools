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
	"bytes"
	"errors"
	"fmt"
	"math/big"
	"math/rand"
	"strings"
	"time"

	"github.com/ethereum/go-ethereum/rlp"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/crypto"

	"github.com/ethereum/go-ethereum/consensus/istanbul"

	"github.com/ethereum/go-ethereum/core"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/params"
)

const (
	GenesisFileName = "genesis.json"
	InitGasLimit    = 4700000
	InitDifficulty  = 1
)

type configureGenesisFn func(genesis *core.Genesis, nodes []*Node, consensusConfig map[string]string) error

var genesisConfigurerByConsensus = map[string]configureGenesisFn{
	// we might need to extract this logic out so we can provide a utility for istanbul
	"istanbul": func(genesis *core.Genesis, nodes []*Node, consensusConfig map[string]string) error {
		genesis.Mixhash = types.IstanbulDigest
		genesis.Config.Istanbul = &params.IstanbulConfig{
			ProposerPolicy: uint64(istanbul.DefaultConfig.ProposerPolicy),
			Epoch:          istanbul.DefaultConfig.Epoch,
		}
		if validatorIdx, ok := consensusConfig["validators"]; ok {
			extraData := types.IstanbulExtra{
				Validators:    make([]common.Address, 0),
				CommittedSeal: [][]byte{},
				Seal:          make([]byte, types.IstanbulExtraSeal),
			}
			for idx, n := range nodes {
				if strings.Contains(validatorIdx, fmt.Sprintf("%d", idx)) {
					extraData.Validators = append(extraData.Validators, crypto.PubkeyToAddress(n.NodeKey.PublicKey))
				}
			}
			vanity := bytes.Repeat([]byte{0x00}, types.IstanbulExtraVanity)
			payload, err := rlp.EncodeToBytes(&extraData)
			if err != nil {
				return err
			}
			genesis.ExtraData = []byte(fmt.Sprintf("0x%s", common.Bytes2Hex(append(vanity, payload...))))
		} else {
			return errors.New("istanbul requires config.validators to be configured")
		}
		return nil
	},
}

func NewGenesis(nodes []*Node, consensus string, consensusConfig map[string]string) (*core.Genesis, error) {
	genesis := &core.Genesis{
		Timestamp:  uint64(time.Now().Unix()),
		GasLimit:   InitGasLimit,
		Difficulty: big.NewInt(InitDifficulty),
		Alloc:      make(core.GenesisAlloc),
		Config: &params.ChainConfig{
			ChainId:        big.NewInt(rand.Int63()),
			HomesteadBlock: big.NewInt(1),
			EIP150Block:    big.NewInt(2),
			EIP155Block:    big.NewInt(3),
			EIP158Block:    big.NewInt(3),
			IsQuorum:       true,
		},
	}
	for _, n := range nodes {
		genesis.Alloc[n.DefaultAccount.AccountAddress] = core.GenesisAccount{Balance: big.NewInt(1).Exp(big.NewInt(10), big.NewInt(50), nil)}
	}
	if fn, ok := genesisConfigurerByConsensus[consensus]; ok {
		if err := fn(genesis, nodes, consensusConfig); err != nil {
			return nil, err
		}
	}
	return genesis, nil
}
