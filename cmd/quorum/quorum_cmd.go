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
	"os"

	"github.com/jpmorganchase/quorum-tools/docker"
	"github.com/spf13/cobra"
)

type cmdArgs struct {
	configFile string
}

var cmdCfg = new(cmdArgs)

var supportedConsensus = map[string]struct{}{
	"raft":     {},
	"istanbul": {},
}

var Cmd = &cobra.Command{
	Use:               "quorum",
	Short:             "Create a local Quorum Network using Docker",
	Long:              ``,
	PersistentPreRunE: cmdCfg.validate,
}

func init() {
	Cmd.PersistentFlags().StringVarP(&(cmdCfg.configFile), "file", "f", "quorum.yml", "Config file describing the network")

	Cmd.AddCommand(upCmd)
	Cmd.AddCommand(downCmd)
}

func (cfg *cmdArgs) validate(cmd *cobra.Command, args []string) error {
	if err := cmd.Parent().PersistentPreRunE(cmd.Parent(), args); err != nil {
		return err
	}
	f, err := os.Open(cfg.configFile)
	if err != nil {
		return err
	}
	defer f.Close()
	docker.CurrrentBuilder, err = docker.NewQuorumBuilder(f)
	if err != nil {
		return err
	}
	return nil
}
