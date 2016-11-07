#!/bin/bash

set -u
set -e

node="$1"

gethpid=

function cleanup {
    echo "Cleaning up"
    kill $gethpid &
}

trap cleanup EXIT

args="--networkid 1418 --nodiscover --nat none --maxpeers 10 --rpc --rpccorsdomain '*' --rpcaddr localhost --fakepow --premine --blocktime 0 --blockjitter 0"

geth_cmd="geth --datadir gdata/geth$node --port 3040$node --rpcport 4040$node $args $@"

$geth_cmd >geth$node.out 2>&1 &
gethpid=$!

# Connect the peers
sleep 2
gethId=$(geth --exec "admin.nodeInfo.enode;" attach ipc:gdata/geth$node/geth.ipc)

echo "To add this peer from another node:"
echo 'admin.addPeer('$gethId');'

read -n1 -r -p "Starting raft when ready; press [space] to continue..." key

# echo "Then, start raft:"
# echo 'geth --exec "raft.startNode();" attach ipc:gdata/geth$node/geth.ipc'

geth --exec "raft.startNode();" attach ipc:gdata/geth$node/geth.ipc

# Wait forever
cat
