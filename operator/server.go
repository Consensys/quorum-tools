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

package operator

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"time"

	"github.com/jpmorganchase/quorum-tools/operator/apiv1"

	"github.com/jpmorganchase/quorum-tools/docker"

	"github.com/ethereum/go-ethereum/log"
	"github.com/gorilla/mux"
)

var v1 *apiv1.API

func Start(listenAddress string, port int, qn *docker.QuorumNetwork) error {
	defer func() {
		if docker.CurrrentBuilder != nil {
			docker.CurrrentBuilder.Destroy(true)
		}
	}()
	v1 = &apiv1.API{
		QuorumNetwork: qn,
		Mux:           new(sync.RWMutex),
	}
	router := mux.NewRouter()
	if qn == nil {
		setupCreatorHandlers(router)
	} else {
		setupHandlers(router)
	}
	server := &http.Server{
		Addr:         fmt.Sprintf("%s:%d", listenAddress, port),
		WriteTimeout: 5 * time.Minute,
		ReadTimeout:  5 * time.Minute,
		IdleTimeout:  5 * time.Minute,
		Handler:      router,
	}
	if port == 0 {
		listener, err := net.Listen("tcp", fmt.Sprintf("%s:%d", listenAddress, port))
		if err != nil {
			return err
		}
		port = listener.Addr().(*net.TCPAddr).Port
		go func() {
			if err := server.Serve(listener); err != nil {
				log.Info("Stop Quorum Network Operator", "reason", err)
			}
		}()
	} else {
		go func() {
			if err := server.ListenAndServe(); err != nil {
				log.Info("Stop Quorum Network Operator", "reason", err)
			}
		}()
	}
	log.Info("Start Quorum Network Operator", "listen", listenAddress, "port", port)
	v1.Port = port

	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)

	<-c // wait

	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Second)
	defer cancel()

	return server.Shutdown(ctx)
}

func setupCreatorHandlers(r *mux.Router) {
	v1Router := r.PathPrefix("/v1").Subrouter()

	v1Router.HandleFunc("/networks", v1.NewNetwork).Methods("POST")
	v1Router.HandleFunc("/networks/{name}", v1.DeleteNetwork).Methods("DELETE")
}

func setupHandlers(r *mux.Router) {
	// /v1 endpoint and accept application/json only
	v1Router := r.PathPrefix("/v1").Subrouter()

	// /v1/nodes endpoint
	nodesRouter := v1Router.PathPrefix("/nodes").Subrouter()
	nodesRouter.HandleFunc("", v1.GetNodes).Methods("GET")
	nodesRouter.HandleFunc("/", v1.GetNodes).Methods("GET")
	nodesRouter.HandleFunc("", v1.AddNodes).Methods("PUT")
	nodesRouter.HandleFunc("/", v1.AddNodes).Methods("PUT")
	nodesRouter.HandleFunc("/{idx}", v1.GetNode).Methods("GET")
	nodesRouter.HandleFunc("/{idx}", v1.ActionOnNode).Methods("POST")
	nodesRouter.HandleFunc("/{idx}/{target}/logs", v1.StreamLogs).Methods("GET")
}
