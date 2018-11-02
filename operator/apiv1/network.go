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
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"time"

	"github.com/ethereum/go-ethereum/log"
	"github.com/jpmorganchase/quorum-tools/helper"
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
	freePort := api.getFreePort()
	// run qctl quorum -f <tmpFile> up --enable-operator --operator-port 0
	qctlCommand := exec.Command(qctl, "quorum", "-f", tmpFile.Name(), "up", "--enable-operator", "--operator-port", fmt.Sprintf("%d", freePort))
	qctlCommand.Stdout = os.Stdout
	qctlCommand.Stderr = os.Stdin
	if err := qctlCommand.Start(); err != nil {
		log.Info("Command run failed", "error", err)
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
	address := fmt.Sprintf("%s:%d", regexp.MustCompile(":.+").ReplaceAllString(r.Host, ""), freePort)
	waitForServer := &helper.StateChangeConfig{
		PollInterval: 3 * time.Second,
		Timeout:      5 * time.Minute,
		Target:       []string{"up"},
		Refresh: func() (*helper.StateResult, error) {
			conn, err := net.Dial("tcp", address)
			if err != nil {
				return &helper.StateResult{
					State: "not up",
				}, nil
			}
			defer conn.Close()
			return &helper.StateResult{
				State:  "up",
				Result: "up",
			}, nil
		},
	}
	if _, err := waitForServer.Wait(); err != nil {
		log.Info("Network is not up", "error", err)
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
	if err := writeJSON(w, map[string]string{
		"operator-address": fmt.Sprintf("http://%s", address),
	}); err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}
}

func (api *API) DeleteNetwork(w http.ResponseWriter, r *http.Request) {

}
