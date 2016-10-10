#!/bin/bash

set -u
set -e

die () {
    echo >&2 "$@"
    exit 1
}

usage () {
    die "example usage: $0 [send | flood] geth1 geth2 5"
}

[ "$#" -eq 4 ] || usage

cmd=$1
from=$2
to=$3
amount=$4

# Until we fix `admin.peers` in the JS console, we need to get these on the bash side:
geth1Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth1/geth.ipc)
geth2Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth2/geth.ipc)
geth3Id=$(geth --exec "eth.accounts[0];" attach ipc:gdata/geth3/geth.ipc)

loadScript="loadScript('send.js'); registerAccounts({'geth1': $geth1Id, 'geth2': $geth2Id, 'geth3': $geth3Id})"

if [ "send" = "$cmd" ]; then
    echo "sending $amount ether from $from to $to"

    geth --exec "$loadScript; sendTo('$to', $amount)" attach ipc:gdata/$from/geth.ipc
elif [ "flood" = "$cmd" ]; then
    echo "flooding with $amount messages from $from to $to"

    geth --exec "$loadScript; floodTo('$to', $amount)" attach ipc:gdata/$from/geth.ipc
else
    usage
fi
