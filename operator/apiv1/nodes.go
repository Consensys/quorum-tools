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
	"fmt"
	"io/ioutil"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/google/uuid"

	"github.com/gorilla/websocket"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/jpmorganchase/quorum-tools/docker"

	"github.com/ethereum/go-ethereum/log"

	"github.com/gorilla/mux"
)

type nodeOutput struct {
	Url              string `json:"url"`
	PrivacyAddress   string `json:"privacy-address"`
	EnodeAddress     string `json:"enode-address"`
	ValidatorAddress string `json:"validator-address,omitempty"` // for istanbul
}

type logConsumer struct {
	id            string
	quorumNetwork *docker.QuorumNetwork
	source        int    // container index to follow
	sourceType    string // quorum or tx_manager

	conn *websocket.Conn

	receiveChan chan []byte
	pingPeriod  time.Duration
	writeWait   time.Duration
}

type logBroadcaster struct {
	source        int
	sourceType    string
	quorumNetwork *docker.QuorumNetwork

	consumers map[*logConsumer]bool

	broadcastChan  chan []byte
	registerChan   chan *logConsumer
	unregisterChan chan *logConsumer
	stopChan       chan struct{}
}

type logHub struct {
	broadcasters   map[int]*logBroadcaster
	registerChan   chan *logConsumer
	unregisterChan chan *logConsumer
	mux            *sync.RWMutex
}

type enrichNodeOutputFn func(quorm *docker.Quorum, txManager docker.TxManager, nodeOut *nodeOutput)

var (
	enrichNodeOutputByConsensus = map[string]enrichNodeOutputFn{
		"istanbul": func(quorum *docker.Quorum, txManager docker.TxManager, nodeOut *nodeOutput) {
			nodeOut.ValidatorAddress = crypto.PubkeyToAddress(quorum.NodeKey().PublicKey).Hex()
		},
	}
	wsUpgrader      = websocket.Upgrader{}
	logStreamingHub = &logHub{
		broadcasters:   make(map[int]*logBroadcaster),
		registerChan:   make(chan *logConsumer),
		unregisterChan: make(chan *logConsumer),
		mux:            new(sync.RWMutex),
	}
)

func init() {
	go logStreamingHub.run()
}

func (lh *logHub) wait() {
	lh.mux.Lock()
	defer lh.mux.Unlock()
}

func (lh *logHub) run() {
	lh.mux.Lock()
	defer lh.mux.Unlock()

	for {
		select {
		case c := <-lh.registerChan:
			if b, ok := lh.broadcasters[c.source]; ok {
				b.registerChan <- c
			} else {
				broadcaster := &logBroadcaster{
					source:         c.source,
					sourceType:     c.sourceType,
					quorumNetwork:  c.quorumNetwork,
					consumers:      make(map[*logConsumer]bool),
					broadcastChan:  make(chan []byte),
					registerChan:   make(chan *logConsumer),
					unregisterChan: make(chan *logConsumer),
					stopChan:       make(chan struct{}),
				}
				lh.broadcasters[c.source] = broadcaster
				go broadcaster.run()
				broadcaster.registerChan <- c
			}
		case c := <-lh.unregisterChan:
			if b, ok := lh.broadcasters[c.source]; ok {
				b.unregisterChan <- c
			}
		}
	}
}

func (lb *logBroadcaster) stop() {
	lb.stopChan <- struct{}{}
}

func (lb *logBroadcaster) run() {
	// start container streaming
	go func() {
		if err := lb.quorumNetwork.StreamLogs(lb.source, lb.sourceType, lb.broadcastChan); err != nil {
			log.Error("Unable to stream logs", "error", err)
		}
	}()
	for {
		select {
		case c := <-lb.registerChan:
			log.Debug("Register new log consumer", "target", c.sourceType, "idx", c.source, "consumer", c.id)
			lb.consumers[c] = true
		case c := <-lb.unregisterChan:
			if _, ok := lb.consumers[c]; ok {
				log.Debug("Unregister log consumer", "target", c.sourceType, "idx", c.source, "consumer", c.id)
				delete(lb.consumers, c)
				c.unregister()
			}
		case msg := <-lb.broadcastChan:
			for c := range lb.consumers {
				go func(_c *logConsumer) {
					defer func() {
						if x := recover(); x != nil {
							lb.unregisterChan <- _c
						}
					}()
					_c.receiveChan <- msg
				}(c)
			}
		case <-lb.stopChan:
			close(lb.broadcastChan)
			return
		}
	}
}

func (lc *logConsumer) unregister() {
	defer func() {
		recover() // race condition when we close the channel twice
	}()
	close(lc.receiveChan)
}

func (lc *logConsumer) run() {
	logger := log.New("target", lc.sourceType, "idx", lc.source, "consumer", lc.id)
	ticker := time.NewTicker(lc.pingPeriod)
	defer func() {
		logger.Debug("Log consumer exited")
		close(lc.receiveChan)
		ticker.Stop()
		lc.conn.Close()
	}()
	for {
		select {
		case msg, ok := <-lc.receiveChan:
			if !ok {
				logger.Debug("Receive Channel closed for log consumer")
				return
			}
			lc.conn.SetWriteDeadline(time.Now().Add(lc.writeWait))
			w, err := lc.conn.NextWriter(websocket.TextMessage)
			if err != nil {
				logger.Debug("Unable to get next websocket writer", "error", err)
				return
			}
			w.Write(msg)
			for i := 0; i < len(lc.receiveChan); i++ {
				if msg, ok := <-lc.receiveChan; ok {
					w.Write(msg)
				}
			}
			if err := w.Close(); err != nil {
				logger.Debug("Write message to websocket failed", "error", err)
				return
			}
		case <-ticker.C:
			lc.conn.SetWriteDeadline(time.Now().Add(lc.writeWait))
			if err := lc.conn.WriteMessage(websocket.PingMessage, []byte{}); err != nil {
				logger.Debug("Ping error", "error", err)
				return
			}
		}
	}
}

// GET /v1/nodes
func (api *API) GetNodes(w http.ResponseWriter, r *http.Request) {
	output := make(map[string]*nodeOutput)
	writeFn := writeJSON
	if strings.Contains(r.Header.Get("Accept"), "application/yaml") {
		writeFn = func(w http.ResponseWriter, _ interface{}) error {
			return api.QuorumNetwork.WriteNetworkConfigurationYAML(w)
		}
	} else {
		host := regexp.MustCompile(":.+").ReplaceAllString(r.Host, "")
		for i := 0; i < api.QuorumNetwork.NodeCount; i++ {
			output[fmt.Sprintf("Node%d", i+1)] = api.buildNodeOutput(i, host)
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
		host := regexp.MustCompile(":.+").ReplaceAllString(r.Host, "")
		if err := writeJSON(w, api.buildNodeOutput(idx, host)); err != nil {
			log.Error("Unable to write output", "error", err)
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		}
	} else {
		http.Error(w, "Missing node index", http.StatusBadRequest)
	}
}

// PUT /v1/nodes
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
		output := make(map[string]*nodeOutput, len(nodeIds))
		host := regexp.MustCompile(":.+").ReplaceAllString(r.Host, "")
		for i := 0; i < len(nodeIds); i++ {
			output[fmt.Sprintf("Node%d", nodeIds[i]+1)] = api.buildNodeOutput(nodeIds[i], host)
		}
		if err := writeJSON(w, output); err != nil {
			log.Error("Unable to write output", "error", err)
			http.Error(w, "Internal Server Error", http.StatusInternalServerError)
		}
	}
}

// POST /v1/nodes/{idx}
func (api *API) ActionOnNode(w http.ResponseWriter, r *http.Request) {
	pathVars := mux.Vars(r)
	if a, ok := pathVars["idx"]; ok {
		idx, err := strconv.Atoi(a)
		if err != nil || idx >= api.QuorumNetwork.NodeCount {
			http.Error(w, "Invalid node index", http.StatusBadRequest)
			return
		}
		data, err := ioutil.ReadAll(r.Body)
		if err != nil {
			log.Error("Unable to read request body", "error", err)
			http.Error(w, "Unable to read body", http.StatusInternalServerError)
			return
		}
		actionMap := make(map[string]string)
		if err := json.Unmarshal(data, &actionMap); err != nil {
			log.Error("Unable to marshall request body", "error", err)
			http.Error(w, "Invalid json", http.StatusBadRequest)
			return
		}
		action, hasAction := actionMap["action"]
		target, hasTarget := actionMap["target"]
		if !hasAction || !strings.Contains("stop start restart", strings.Replace(action, " ", "", -1)) {
			log.Error("Unknown action", "action", action)
			http.Error(w, "Unknown action", http.StatusBadRequest)
			return
		}
		if !hasTarget || !strings.Contains("quorum tx_manager", strings.Replace(target, " ", "", -1)) {
			log.Error("Unknown target", "target", target)
			http.Error(w, "Unknown target", http.StatusBadRequest)
			return
		}
		if err := api.QuorumNetwork.PerformAction(idx, target, action); err != nil {
			log.Error("Unable to perform action", "action", action, "error", err)
			http.Error(w, "Unable to perform action", http.StatusInternalServerError)
			return
		}
	} else {
		http.Error(w, "Missing node index", http.StatusBadRequest)
	}
}

// Websocket /v1/nodes/{idx}/{target}/logs
func (api *API) StreamLogs(w http.ResponseWriter, r *http.Request) {
	pathVars := mux.Vars(r)
	if a, ok := pathVars["idx"]; ok {
		idx, err := strconv.Atoi(a)
		if err != nil || idx >= api.QuorumNetwork.NodeCount {
			http.Error(w, "Invalid node index", http.StatusBadRequest)
			return
		}
		target, hasTarget := pathVars["target"]
		if !hasTarget || !strings.Contains("quorum tx_manager", strings.Replace(target, " ", "", -1)) {
			log.Error("Unknown target", "target", target)
			http.Error(w, "Unknown target", http.StatusBadRequest)
			return
		}
		clientId := uuid.New().String()
		if conn, err := wsUpgrader.Upgrade(w, r, http.Header{"X-Consumer": []string{clientId}}); err != nil {
			log.Error("Unable to initialize websocket connection", "error", err)
			http.Error(w, "Unable to initialize websocket connection", http.StatusInternalServerError)
			return
		} else {
			client := &logConsumer{
				id:            clientId,
				quorumNetwork: api.QuorumNetwork,
				source:        idx,
				sourceType:    target,
				receiveChan:   make(chan []byte),
				conn:          conn,
				pingPeriod:    3 * time.Second,
				writeWait:     10 * time.Second,
			}
			logStreamingHub.registerChan <- client
			go client.run()
			defer func() { logStreamingHub.unregisterChan <- client }()
			logStreamingHub.wait()
		}
	}
}

func (api *API) buildNodeOutput(i int, host string) *nodeOutput {
	q := api.QuorumNetwork.QuorumNodes[i]
	if q == nil {
		return &nodeOutput{}
	}
	nodeOut := &nodeOutput{
		Url:            q.Url(host),
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
