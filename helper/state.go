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

package helper

import (
	"errors"
	"time"

	"github.com/ethereum/go-ethereum/log"
)

type StateResult struct {
	Result interface{}
	State  string
}

type StateRefreshFunc func() (*StateResult, error)

type StateChangeConfig struct {
	Target       []string         // states that once matched, program will stop
	Refresh      StateRefreshFunc // refresh the current state
	Timeout      time.Duration
	PollInterval time.Duration // wait between refreshes
}

func (conf *StateChangeConfig) Wait() (interface{}, error) {
	log.Debug("Wait for state", "target", conf.Target)
	resultChan := make(chan *StateResult)
	errChan := make(chan error)
	go func() {
		for {
			time.Sleep(conf.PollInterval)
			result, err := conf.Refresh()
			if err == nil {
				log.Trace("Current state", "state", result.State)
				for _, target := range conf.Target {
					if target == result.State {
						resultChan <- result
						return
					}
				}
			} else {
				errChan <- err
			}
		}
	}()
	timeout := time.After(conf.Timeout)
	for {
		select {
		case err := <-errChan:
			return nil, err
		case result := <-resultChan:
			return result.Result, nil
		case <-timeout:
			log.Info("Wait time out", "target", conf.Target, "after", conf.Timeout)
			return nil, errors.New("timeout while waiting for target states")
		}
	}
}
