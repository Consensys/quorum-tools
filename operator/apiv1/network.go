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
	"syscall"
	"time"

	"github.com/gorilla/mux"

	"github.com/jpmorganchase/quorum-tools/docker"

	"github.com/ethereum/go-ethereum/log"
	"github.com/jpmorganchase/quorum-tools/helper"
)

type networkCache struct {
	builder         *docker.QuorumBuilder
	operatorAddress string
	qctlCommand     *exec.Cmd
}

var cache = make(map[string]*networkCache)

// POST /v1/networks
func (api *API) NewNetwork(w http.ResponseWriter, r *http.Request) {
	tmpFile, err := ioutil.TempFile("", "conf.*.yaml")
	if err != nil {
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
	defer r.Body.Close()
	if _, err := io.Copy(tmpFile, r.Body); err != nil {
		http.Error(w, "Can't write", http.StatusInternalServerError)
		return
	}
	f, err := os.Open(tmpFile.Name())
	if err != nil {
		http.Error(w, "Can't read", http.StatusInternalServerError)
		return
	}
	defer f.Close()
	builder, err := docker.NewQuorumBuilder(f)
	if err != nil {
		http.Error(w, "Invalid template format", http.StatusBadRequest)
		return
	}
	log.Info("Create new network", "name", builder.Name)
	var address string
	if nc, ok := api.getFromCache(builder.Name); ok {
		address = nc.operatorAddress
	} else {
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
		qctlCommand.Stderr = os.Stderr
		if err := qctlCommand.Start(); err != nil {
			log.Info("Command run failed", "error", err)
			http.Error(w, "Server error", http.StatusInternalServerError)
			return
		}
		dialAddress := fmt.Sprintf("%s:%d", regexp.MustCompile(":.+").ReplaceAllString(r.Host, ""), freePort)
		address = fmt.Sprintf("http://%s", dialAddress)
		waitForServer := &helper.StateChangeConfig{
			PollInterval: 3 * time.Second,
			Timeout:      5 * time.Minute,
			Target:       []string{"up"},
			Refresh: func() (*helper.StateResult, error) {
				conn, err := net.Dial("tcp", dialAddress)
				if err != nil {
					return &helper.StateResult{
						State: "not up",
					}, nil
				}
				defer conn.Close()
				if err := qctlCommand.Process.Signal(syscall.Signal(0)); err != nil {
					return nil, err
				}
				return &helper.StateResult{
					State:  "up",
					Result: "up",
				}, nil
			},
		}
		if _, err := waitForServer.Wait(); err != nil {
			log.Info("Network is not up. Clean up", "error", err)
			if err := exec.Command(qctl, "quorum", "-f", tmpFile.Name(), "down").Run(); err != nil {
				log.Info("Can't clean up network", "error", err)
			}
			http.Error(w, "Server error", http.StatusInternalServerError)
			return
		}
		cache[builder.Name] = &networkCache{
			builder:         builder,
			operatorAddress: address,
			qctlCommand:     qctlCommand,
		}
	}
	if err := writeJSON(w, map[string]string{
		"operator-address": address,
	}); err != nil {
		http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		return
	}
}

// DELETE /v1/networks/{name}
func (api *API) DeleteNetwork(w http.ResponseWriter, r *http.Request) {
	pathVars := mux.Vars(r)
	name, ok := pathVars["name"]
	if !ok {
		http.Error(w, "Missing network name", http.StatusBadRequest)
		return
	}
	c, ok := cache[name]
	if !ok {
		http.Error(w, "No such network", http.StatusBadRequest)
		return
	}
	if err := c.builder.Destroy(true); err != nil {
		http.Error(w, "Server error", http.StatusInternalServerError)
		return
	}
	if err := c.qctlCommand.Process.Signal(os.Interrupt); err != nil {
		c.qctlCommand.Process.Signal(os.Kill)
	}
	delete(cache, name)
}

func (api *API) getFromCache(name string) (nc *networkCache, ok bool) {
	api.Mux.Lock()
	defer api.Mux.Unlock()
	nc, ok = cache[name]
	return
}
