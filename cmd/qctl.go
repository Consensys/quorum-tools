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

package cmd

import (
	"fmt"
	"os"

	"github.com/ethereum/go-ethereum/log"

	"github.com/jpmorganchase/quorum-tools/cmd/quorum"

	"github.com/spf13/cobra"
)

var (
	Verbosity int
)

var rootCmd = &cobra.Command{
	Use:   "qctl",
	Short: "qctl provides a set of tools for Quorum",
	Long: `Quorum is a permissioned implementation of Ethereum supporting data privacy.
qctl provides a set of tools to work with Quorum.`,
	PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
		if log.Lvl(Verbosity) == log.LvlTrace {
			log.PrintOrigins(true)
		}
		glogger := log.NewGlogHandler(log.StreamHandler(os.Stdout, log.TerminalFormat(false)))
		glogger.Verbosity(log.Lvl(Verbosity))
		glogger.BacktraceAt("")
		glogger.Vmodule("")
		log.Root().SetHandler(glogger)
		return nil
	},
}

func init() {
	rootCmd.PersistentFlags().IntVarP(&Verbosity, "verbosity", "v", 3, "Logging verbosity: 0=silent, 1=error, 2=warn, 3=info, 4=debug, 5=detail")
	rootCmd.AddCommand(quorum.Cmd)
}

func Execute() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
