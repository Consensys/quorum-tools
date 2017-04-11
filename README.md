# Quorum-Raft testing / deployment framework

This repo contains the tools we use for both testing Quorum-Raft and deploying
it to AWS.

## Building the binaries

If you haven't built this project yet, make sure that you have the GHC compiler:

```
$ stack setup
```

Now you can build the project:

```
$ stack build
```

At this point, you can run any of the built binaries using `stack exec`:

```
$ stack exec -- local-new
```

Or you can use `stack install` to install them on your machine (make sure `~/.local/bin` is on your `$PATH`):

```
$ stack install
$ local-new
```

## Deployment

We use [docker](https://www.docker.com/) to build images for AWS and [Terraform](https://www.terraform.io/) to deploy them. Using Terraform, it should be possible to deploy to any host with docker support, but this hasn't been tested.


### Example

We'll start with an example of the basic deployment workflow, then detail what all the pieces do.

```console
$ ./build-all && ./push
Sending build context to Docker daemon 4.608 kB
Step 1/7 : FROM ubuntu:xenial
 ---> 12543ced0f6f
... (snip) ...
latest: digest: sha256:ac8532679ab67f21b6188a0fb95ca18f781386ebbf98a0d2498fc762f252f6ac size: 3038
$ cd terraform/bin
$ # symlink env-wrapper to create a convenient deployment script wrapping terraform
$ ln -s .bin/env-wrapper demo
$ cd ..
$ ./bin/demo plan
Refreshing Terraform state in-memory prior to plan...
... (snip) ...
$ ./bin/demo apply
null_resource.cluster_datadirs: Refreshing state... (ID: 2861555940766274283)
... (snip) ...
Apply complete! Resources: 0 added, 0 changed, 0 destroyed.

Outputs:

environment = demo
geth1 = 54.174.195.39
geth2 = 54.85.33.206
geth3 = 54.196.76.138
quorum-azs = [
    us-east-1b,
    us-east-1c,
    us-east-1d
]
quorum-private-ips = [
    10.0.1.101,
    10.0.2.101,
    10.0.3.101
]
quorum-public-ips = [
    54.174.195.39,
    54.85.33.206,
    54.196.76.138
]
$ # Now we have a cluster running across three availability zones. We can ssh in to one of the machines to inspect our running cluster.
$ ssh ubuntu@(./bin/profile output geth2) # TODO(joel) bash syntax?
$ sudo docker stats
...
```

### Scripts

First, scripts dealing with building and deploying docker images:

* `build`
* `build-all`
* `build-harness`
* `build-quorum`
* `push`

Build and run an image:

```console
./build
docker run -it quorum-raft
```

Next, scripts for running the cluster:

* `local-new`
* `local-start`
* `local-spam`
* `aws-bootstrap`
* `aws-spam`

## Tests

This repo also includes tests of a quorum-like system:

* A network partition of one node in a running cluster
  - Including the case where this node is the Raft leader
* Public and private state consistency
* Completely stopping and restarting a node in a running cluster.

The test sources are located in `src/` / `app/`.
