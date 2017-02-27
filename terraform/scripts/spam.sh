#!/bin/bash

set -e
set -u

die() {
    echo >&2 "$@"
    exit 1
}

print_usage() {
    die "usage: spam RPS"
}

[ "$#" -ne 1 ] && print_usage

rps=$1
cluster_type=$(cat cluster-type)

if [[ $cluster_type == "multi-region" ]]
then
    multi_region_opt="-g"
else
    multi_region_opt=""
fi

sudo docker run -it -v /home/ubuntu/datadir:/datadir -v /home/ubuntu/node-id:/home/ubuntu/node-id 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-harness /bin/sh -c "cd /root/gemini-examples/raft-demo && stack exec -- aws-spam -r ${rps} ${multi_region_opt}"
