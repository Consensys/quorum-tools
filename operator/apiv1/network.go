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
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"

	"github.com/ethereum/go-ethereum/log"
)

// POST /v1/networks
func (api *API) NewNetwork(w http.ResponseWriter, r *http.Request) {
	tmpFile, err := ioutil.TempFile("", "conf.*.yaml")
	if err != nil {
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
	if _, err := io.Copy(tmpFile, r.Body); err != nil {
		http.Error(w, "Can't write", http.StatusInternalServerError)
		return
	}
	log.Info("Write network config", "file", tmpFile.Name())
	qctl, err := os.Executable()
	if err != nil {
		http.Error(w, "Can't run network creator", http.StatusInternalServerError)
		return
	}
	// run qctl quorum -f <tmpFile> up --enable-operator --operator-port 0
	qctlCommand := exec.Command(qctl, "quorum", "-f", tmpFile.Name(), "up", "--enable-operator", "--operator-port", "0")
	qctlCommand.Stdout = os.Stdout
	qctlCommand.Stderr = os.Stdin
	if err := qctlCommand.Start(); err != nil {
		log.Info("Command run failed", "error", err)
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
}

func (api *API) DeleteNetwork(w http.ResponseWriter, r *http.Request) {

}
