#!/bin/bash

set -e
set -u

# NOTE: this only works for clusters of size 9 or smaller:
ID=$(cat node-id)

echo "starting geth ${ID}"

sudo docker run -d -p 3040$ID:3040$ID -p 4040$ID:4040$ID -p 5040$ID:5040$ID -v /home/ubuntu/datadir:/datadir -v /home/ubuntu/password:/password -e PRIVATE_CONFIG='/datadir/constellation.toml' 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-raft /bin/sh -c "geth --datadir /datadir --port 3040$ID --rpcport 4040$ID --networkid 1418 --verbosity 3 --nodiscover --rpc --rpccorsdomain '*' --rpcaddr '0.0.0.0' --raft --unlock 0 --password /password"
