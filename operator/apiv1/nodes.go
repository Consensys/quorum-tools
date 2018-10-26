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

package apiv1

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/jpmorganchase/quorum-tools/docker"

	"github.com/ethereum/go-ethereum/log"

	"github.com/gorilla/mux"
)

type nodeOutput struct {
	Id               int    `json:"id"`
	Url              string `json:"url"`
	PrivacyAddress   string `json:"privacy-address"`
	EnodeAddress     string `json:"enode-address"`
	ValidatorAddress string `json:"validator-address,omitempty"` // for istanbul
}

type enrichNodeOutputFn func(quorm *docker.Quorum, txManager docker.TxManager, nodeOut *nodeOutput)

var (
	enrichNodeOutputByConsensus = map[string]enrichNodeOutputFn{
		"istanbul": func(quorum *docker.Quorum, txManager docker.TxManager, nodeOut *nodeOutput) {
			nodeOut.ValidatorAddress = crypto.PubkeyToAddress(quorum.NodeKey().PublicKey).Hex()
		},
	}
)

// GET /v1/nodes
func (api *API) GetNodes(w http.ResponseWriter, r *http.Request) {
	output := make([]*nodeOutput, api.QuorumNetwork.NodeCount)
	writeFn := writeJSON
	if strings.Contains(r.Header.Get("Accept"), "application/yaml") {
		writeFn = func(w http.ResponseWriter, _ interface{}) error {
			return api.QuorumNetwork.WriteNetworkConfigurationYAML(w)
		}
	} else {
		for i := 0; i < api.QuorumNetwork.NodeCount; i++ {
			output[i] = api.buildNodeOutput(i)
		}
	}
	if err := writeFn(w, output); err != nil {
		log.Error("Unable to write output", "error", err)
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
	}
}

// GET /v1/nodes/{idx}
func (api *API) GetNode(w http.ResponseWriter, r *http.Request) {
	pathVars := mux.Vars(r)
	if a, ok := pathVars["idx"]; ok {
		idx, err := strconv.Atoi(a)
		if err != nil || idx >= api.QuorumNetwork.NodeCount {
			http.Error(w, "Invalid node index", http.StatusBadRequest)
			return
		}
		if err := writeJSON(w, api.buildNodeOutput(idx)); err != nil {
			log.Error("Unable to write output", "error", err)
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		}
	} else {
		http.Error(w, "Missing node index", http.StatusBadRequest)
	}
}

func (api *API) AddNodes(w http.ResponseWriter, r *http.Request) {
	data, err := ioutil.ReadAll(r.Body)
	if err != nil {
		log.Error("Unable to read request body", "error", err)
		http.Error(w, "Unable to read body", http.StatusInternalServerError)
		return
	}
	newNodes := make([]docker.QuorumBuilderNode, 0)
	if err := json.Unmarshal(data, &newNodes); err != nil {
		log.Error("Unable to marshall request body", "error", err)
		http.Error(w, "Invalid json", http.StatusBadRequest)
		return
	}
	if nodeIds, err := api.QuorumNetwork.AddNodes(newNodes); err != nil {
		http.Error(w, "Unable to add nodes", http.StatusInternalServerError)
		return
	} else {
		output := make([]*nodeOutput, len(nodeIds))
		for i := 0; i < len(nodeIds); i++ {
			output[i] = api.buildNodeOutput(nodeIds[i])
		}
		if err := writeJSON(w, output); err != nil {
			log.Error("Unable to write output", "error", err)
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		}
	}
}

func (api *API) buildNodeOutput(i int) *nodeOutput {
	q := api.QuorumNetwork.QuorumNodes[i]
	if q == nil {
		return &nodeOutput{
			Id: i,
		}
	}
	nodeOut := &nodeOutput{
		Id:             i,
		Url:            q.Url(),
		PrivacyAddress: api.QuorumNetwork.TxManagers[i].Address(),
		EnodeAddress:   q.BootstrapData().Enode,
	}
	if fn, ok := enrichNodeOutputByConsensus[q.ConsensusAlgorithm()]; ok {
		fn(q, api.QuorumNetwork.TxManagers[i], nodeOut)
	}
	return nodeOut
}

func writeJSON(w http.ResponseWriter, output interface{}) error {
	if data, err := json.Marshal(output); err != nil {
		return err
	} else {
		w.Header().Set("Content-type", "application/json")
		w.WriteHeader(http.StatusOK)
		w.Write(data)
	}
	return nil
}
