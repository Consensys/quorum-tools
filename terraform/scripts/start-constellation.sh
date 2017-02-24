#!/bin/bash

set -e
set -u

my_gid=$(cat node-id)
cluster_type=$(cat cluster-type)

wait_for_peer() {
    gid=$1

    if [[ $cluster_type == "multi-region" ]]
    then
        endpoint="localhost:900${gid}"
    else
        #
        # FIXME: this only works for clusters up to size <= number of subnets.
        #
        endpoint="10.0.${gid}.101:900${gid}"
    fi

    until (curl -s "http://${endpoint}" >/dev/null); do
        echo "retrying connection to constellation ${gid} at ${endpoint} shortly"
        sleep 2
    done
    echo "successfully connected to constellation ${gid}"
}

echo "about to connect to previous constellations"

# Wait for all peers with a geth ID lower than $my_gid
for i in $(seq 1 $(($my_gid - 1))); do
    wait_for_peer $i
done

# Give the last-started constellation a few seconds to synchronize.
sleep 5

echo "starting constellation ${my_gid}"

# Start this constellation. This method of setting the port only works for
# clusters of size 9 or smaller.
sudo docker run -d \
                -p 900${my_gid}:900${my_gid} \
                -v /home/ubuntu/datadir:/datadir \
                128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-constellation \
                /bin/sh -c "cd /root/constellation && stack exec -- constellation-node /datadir/constellation.toml"
