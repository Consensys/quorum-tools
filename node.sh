#!/bin/bash

set -u
set -e

geth1pid=

function cleanup {
    echo "Cleaning up"
    kill $geth1pid &
}

trap cleanup EXIT
rm -rf gdata

args="--networkid 1418 --nodiscover --nat none --maxpeers 10 --rpc --rpccorsdomain '*' --rpcaddr localhost --fakepow --premine --blocktime 0 --blockjitter 0"

geth1="geth --datadir gdata/geth1 --port 30401 --rpcport 40401 $args $@"

$geth1 init genesis.json

$geth1 --password <(echo abcd) account new

$geth1 >geth1.out 2>&1 &
geth1pid=$!

# Connect the peers
sleep 2
geth1Id=$(geth --exec "admin.nodeInfo.enode;" attach ipc:gdata/geth1/geth.ipc)

echo "To add this peer from another node:"
echo 'admin.addPeer('$geth1Id');'
# echo 'geth --exec "admin.addPeer('$geth1Id');" attach ipc:gdata/geth2/geth.ipc'

sleep 1

echo "Then, start raft:"
echo 'geth --exec "raft.startNode();" attach ipc:gdata/geth1/geth.ipc'

# Wait forever
cat
