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

package bootstrap

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
)

func WritePermissionedNodes(nodes []*Node) error {
	permissions := make([]string, len(nodes))
	for idx, n := range nodes {
		permissions[idx] = fmt.Sprintf("enode://%s@%s:%d?discport=0&raftport=%d", n.Enode, n.IP, n.P2PPort, 0)
	}
	data := new(bytes.Buffer)
	encoder := json.NewEncoder(data)
	encoder.SetEscapeHTML(false)
	if err := encoder.Encode(permissions); err != nil {
		return err
	}
	for _, n := range nodes {
		if err := ioutil.WriteFile(filepath.Join(n.DataDir.GethDir, "static-nodes.json"), data.Bytes(), 0700); err != nil {
			return err
		}
		if err := ioutil.WriteFile(filepath.Join(n.DataDir.GethDir, "permissioned-nodes.json"), data.Bytes(), 0700); err != nil {
			return err
		}
		if err := ioutil.WriteFile(filepath.Join(n.DataDir.Base, "passwords.txt"), []byte(""), 0700); err != nil {
			return err
		}
	}
	return nil
}
