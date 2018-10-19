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
	"fmt"
	"io/ioutil"
	"time"

	"github.com/ethereum/go-ethereum/accounts/keystore"
	"github.com/ethereum/go-ethereum/common"
)

type Account struct {
	KeystoreDir    *string
	AccountAddress common.Address
	Keystore       *keystore.KeyStore
	Passphrase     *string
}

// create a new account with empty passphrase in a new keystore
func NewAccount() (*Account, error) {
	tmp, err := ioutil.TempDir("", fmt.Sprintf("qctl-ks-%d", time.Now().Unix()))
	if err != nil {
		return nil, fmt.Errorf("NewAccount: can't create tmp dir - %s", err)
	}
	ks := keystore.NewKeyStore(tmp, keystore.StandardScryptN, keystore.StandardScryptP)
	passphrase := ""
	acc, err := ks.NewAccount(passphrase)
	if err != nil {
		return nil, fmt.Errorf("NewAccount: can't create new account - %s", err)
	}
	return &Account{
		KeystoreDir:    &tmp,
		AccountAddress: acc.Address,
		Keystore:       ks,
		Passphrase:     &passphrase,
	}, nil
}
