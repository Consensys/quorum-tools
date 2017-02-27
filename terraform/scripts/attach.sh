#!/bin/bash

set -e
set -u

sudo docker run -v /home/ubuntu/datadir:/datadir -it 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-raft /bin/sh -c "geth attach /datadir/geth.ipc"
