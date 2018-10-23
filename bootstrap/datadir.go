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
	"io/ioutil"
	"os"
	"path/filepath"
)

type DataDir struct {
	Base        string
	KeystoreDir string
	GethDir     string
}

func NewDataDir(tmpDir string) (*DataDir, error) {
	dir, err := ioutil.TempDir(tmpDir, "qdata-")
	if err != nil {
		return nil, err
	}
	dd := &DataDir{
		Base:        dir,
		KeystoreDir: filepath.Join(dir, "keystore"),
		GethDir:     filepath.Join(dir, "geth"),
	}
	if err := os.MkdirAll(dd.KeystoreDir, 0700); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(dd.GethDir, 0700); err != nil {
		return nil, err
	}
	return dd, nil
}
