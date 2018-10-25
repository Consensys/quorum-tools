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
	"github.com/ethereum/go-ethereum/log"
	"net/http"
	"strconv"
	"strings"

	"github.com/gorilla/mux"
)

type nodeOutput struct {
	Id             int    `json:"id"`
	Url            string `json:"url"`
	PrivacyAddress string `json:"privacy-address"`
}

// GET /v1/nodes
func (api *API) GetNodes(w http.ResponseWriter, r *http.Request) {
	var output interface{}
	writeFn := writeJSON
	if strings.Contains(r.Header.Get("Accept"), "application/yaml") {
		writeFn = func(w http.ResponseWriter, output interface{}) error {
			return api.QuorumNetwork.WriteNetworkConfigurationYAML(w)
		}
	} else {
		output := make([]nodeOutput, api.QuorumNetwork.NodeCount)
		for i := 0; i < api.QuorumNetwork.NodeCount; i++ {
			output[i] = nodeOutput{
				Id:             i,
				Url:            api.QuorumNetwork.QuorumNodes[i].Url(),
				PrivacyAddress: api.QuorumNetwork.TxManagers[i].Address(),
			}
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
		if err := writeJSON(w, nodeOutput{
			Id:             idx,
			Url:            api.QuorumNetwork.QuorumNodes[idx].Url(),
			PrivacyAddress: api.QuorumNetwork.TxManagers[idx].Address(),
		}); err != nil {
			log.Error("Unable to write output", "error", err)
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		}
	} else {
		http.Error(w, "Missing node index", http.StatusBadRequest)
	}
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
