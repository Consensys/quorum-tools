#!/bin/bash

set -u
set -e

die () {
    echo >&2 "$@"
    exit 1
}

[ "$#" -eq 2 ] || die "example usage: $0 geth1 geth2"

from=$1
to=$2

# Until we fix `admin.peers` in the JS console, we need to get these on the bash side:
geth1Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth1/geth.ipc)
geth2Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth2/geth.ipc)
geth3Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth3/geth.ipc)

geth --exec "loadScript('send.js'); registerAccounts({'geth1': $geth1Id, 'geth2': $geth2Id, 'geth3': $geth3Id}); sendTo('$to')" attach ipc:gdata/$from/geth.ipc
