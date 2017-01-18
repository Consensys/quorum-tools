#!/bin/bash

eval `aws ecr get-login --region us-east-1 | sed 's/^docker/sudo docker/'`
sudo docker pull 128367567151.dkr.ecr.us-east-1.amazonaws.com/quorum-harness:latest
