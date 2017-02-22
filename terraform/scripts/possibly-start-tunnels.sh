#!/bin/bash

set -e
set -u

my_gid=$(cat node-id)
cluster_type=$(cat cluster-type)

start_tunnel() {
    gid=$1
    eip=$2
    port=$3

    echo "starting tunnel to geth ${gid} at ${eip}:${port}"

    # I couldn't figure out how to get docker to cooperate with starting an SSH tunnel inside of it, so we use nohup and background for now:
    nohup bash -c "until (ssh -M -T -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -i .ssh/id_tunneler -N -L 0.0.0.0:${port}:localhost:${port} ubuntu@${eip}); do echo 're-establishing tunnel to geth ${gid}:${port}'; done" &
}

start_tunnels() {
    gid=$1
    eip=$2

    start_tunnel $gid $eip "3040${gid}" # Ethereum P2P
    start_tunnel $gid $eip "5040${gid}" # Raft HTTP
    start_tunnel $gid $eip "900${gid}"  # Constellation
}

if [[ $cluster_type == "multi-region" ]]
then
    if [[ $my_gid != "1" ]]
    then
        start_tunnels 1 34.249.147.200
    fi

    if [[ $my_gid != "2" ]]
    then
        start_tunnels 2 13.112.217.48
    fi

    if [[ $my_gid != "3" ]]
    then
        start_tunnels 3 52.70.197.79
    fi

    sleep 1 # see: http://stackoverflow.com/questions/36207752/how-can-i-start-a-remote-service-using-terraform-provisioning
fi
