#!/bin/bash

set -e
set -u

echo "installing docker and the aws cli"

sudo apt-get update -y >/dev/null
sudo apt-get install -y apt-transport-https ca-certificates >/dev/null
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D >/dev/null
sudo sh -c 'echo "deb https://apt.dockerproject.org/repo ubuntu-xenial main" > /etc/apt/sources.list.d/docker.list' >/dev/null
sudo apt-get update -y >/dev/null
sudo apt-get install -y linux-image-extra-$(uname -r) linux-image-extra-virtual >/dev/null
sudo apt-get install -y docker-engine awscli >/dev/null

echo "installation complete"

# Allow SSH access from other quorum nodes for multi-region setups.
cat /home/ubuntu/.ssh/id_tunneler.pub >> /home/ubuntu/.ssh/authorized_keys
chmod 600 /home/ubuntu/.ssh/id_tunneler.pub
chmod 600 /home/ubuntu/.ssh/id_tunneler
