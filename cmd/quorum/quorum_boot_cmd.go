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

package quorum

import (
	"github.com/jpmorganchase/quorum-tools/operator"
	"github.com/spf13/cobra"
)

var bootCmd = &cobra.Command{
	Use:   "boot",
	Short: "Start a server which can create a managed Quorum Network on demand",
	Long:  "Server provides an API to create a managed Quorum Network",
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		return cmd.Root().PersistentPreRunE(cmd, args)
	},
	RunE: func(cmd *cobra.Command, args []string) error {
		return operator.Start(creatorBindAddress, creatorPort, nil)
	},
}

var (
	creatorPort        int
	creatorBindAddress string
)

func init() {
	bootCmd.Flags().IntVarP(&creatorPort, "port", "p", 8800, "Port listening")
	bootCmd.Flags().StringVarP(&creatorBindAddress, "address", "a", "0.0.0.0", "Listing to address")
}
