#!/bin/bash

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

function get_enode {
    node_number=$1
    eval geth="\$geth${node_number}"
    ${geth} js <(echo "console.log(admin.nodeInfo.enode)")
}

trap cleanup EXIT

args="--networkid 1418 --nodiscover --maxpeers 10 --rpc --rpccorsdomain '*' --rpcaddr localhost"

geth1="geth --datadir gdata/geth1 --port 30401 --rpcport 40401 $args $@"
geth2="geth --datadir gdata/geth2 --port 30402 --rpcport 40402 $args $@"
geth3="geth --datadir gdata/geth3 --port 30403 --rpcport 40403 $args $@"

# SET UP NODES

rm -rf gdata

$geth1 init genesis.json
$geth2 init genesis.json
$geth3 init genesis.json

$geth1 --password <(echo abcd) account new
$geth2 --password <(echo abcd) account new
$geth3 --password <(echo abcd) account new

geth1Id=$(get_enode 1)
geth2Id=$(get_enode 2)
geth3Id=$(get_enode 3)

echo "[\"${geth2Id}\", \"${geth3Id}\"]" > gdata/geth1/static-nodes.json
echo "[\"${geth1Id}\", \"${geth3Id}\"]" > gdata/geth2/static-nodes.json
echo "[\"${geth1Id}\", \"${geth2Id}\"]" > gdata/geth3/static-nodes.json

# START NODES

$geth1 >geth1.out 2>&1 &
# $geth1 >/dev/null 2>&1 &
geth1pid=$!

$geth2 >geth2.out 2>&1 &
# $geth2 >/dev/null 2>&1 &
geth2pid=$!

$geth3 >geth3.out 2>&1 &
# $geth3 >/dev/null 2>&1 &
geth3pid=$!

sleep 1

geth --exec "raft.startNode();" attach ipc:gdata/geth1/geth.ipc
geth --exec "raft.startNode();" attach ipc:gdata/geth2/geth.ipc
geth --exec "raft.startNode();" attach ipc:gdata/geth3/geth.ipc

# Wait forever
cat
