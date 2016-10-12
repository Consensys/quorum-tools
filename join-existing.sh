#!/bin/bash

# Test whether geth1 can join the already started geth2 and geth3

set -u
set -e

geth1pid=
geth2pid=
geth3pid=

function cleanup {
    echo "Cleaning up"
    kill $geth1pid &
    kill $geth2pid &
    kill $geth3pid &
}

trap cleanup EXIT
rm -rf gdata

args="--networkid 1418 --nodiscover --maxpeers 10 --fakepow --premine --blocktime 0 --blockjitter 0"

geth2="geth --datadir gdata/geth2 --port 30402 $args $@"
geth3="geth --datadir gdata/geth3 --port 30403 $args $@"

$geth2 init genesis.json
$geth3 init genesis.json

$geth2 --password <(echo abcd) account new
$geth3 --password <(echo abcd) account new

$geth2 >geth2.out 2>&1 &
geth2pid=$!

$geth3 >geth3.out 2>&1 &
geth3pid=$!

# Connect the peers
sleep 2
geth2Id=$(geth --exec "admin.nodeInfo.enode;" attach ipc:gdata/geth2/geth.ipc)
geth3Id=$(geth --exec "admin.nodeInfo.enode;" attach ipc:gdata/geth3/geth.ipc)

echo "Adding geth2 to geth3: $geth2Id"
geth --exec "admin.addPeer($geth2Id);" attach ipc:gdata/geth3/geth.ipc

sleep 1

geth --exec "raft.startNode();" attach ipc:gdata/geth2/geth.ipc
geth --exec "raft.startNode();" attach ipc:gdata/geth3/geth.ipc

# now start geth1

geth1="geth --datadir gdata/geth1 --port 30401 $args $@"
$geth1 init genesis.json
$geth1 --password <(echo abcd) account new
$geth1 >geth1.out 2>&1 &
geth1pid=$!

sleep 2

geth1Id=$(geth --exec "admin.nodeInfo.enode;" attach ipc:gdata/geth1/geth.ipc)

echo "Adding geth1 to geth2: $geth1Id"
geth --exec "admin.addPeer($geth1Id);" attach ipc:gdata/geth2/geth.ipc
echo "Adding geth1 to geth3: $geth1Id"
geth --exec "admin.addPeer($geth1Id);" attach ipc:gdata/geth3/geth.ipc

sleep 1

geth --exec "raft.startNode();" attach ipc:gdata/geth1/geth.ipc

# Wait forever
cat
