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
	"github.com/jpmorganchase/quorum-tools/docker"
	"github.com/jpmorganchase/quorum-tools/operator"
	"github.com/spf13/cobra"
)

var upCmd = &cobra.Command{
	Use:   "up",
	Short: "Build a network",
	Long:  `Build a network with specified configuration`,
	RunE:  executeUpCmd,
}

var (
	export          string
	enableOperator  bool
	operatorPort    int
	operatorAddress string
)

func init() {
	upCmd.Flags().StringVarP(&export, "export", "e", "", "Export information about the network to a file or stdout (use hyphen)")
	upCmd.Flags().BoolVarP(&enableOperator, "enable-operator", "", false, "Start Operator server")
	upCmd.Flags().IntVarP(&operatorPort, "operator-port", "", 8800, "Operator server port")
	upCmd.Flags().StringVarP(&operatorAddress, "operator-address", "", "0.0.0.0", "Operator server listen address")
}

func executeUpCmd(cmd *cobra.Command, args []string) error {
	qn, err := docker.CurrrentBuilder.Build(export)
	if err != nil {
		docker.CurrrentBuilder.Destroy(true)
		return err
	}
	if enableOperator {
		if err := operator.Start(operatorAddress, operatorPort, qn); err != nil {
			return err
		}
	}
	return err
}
