#!/bin/bash

set -e
set -u

echo "fetching docker images"

eval `aws ecr get-login --region us-east-1 | sed 's/^docker/sudo docker/'`                >/dev/null
sudo docker pull 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-raft:latest          >/dev/null
sudo docker pull 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-constellation:latest >/dev/null
sudo docker pull 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-harness:latest       >/dev/null

echo "fetching complete"
