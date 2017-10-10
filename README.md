# Quorum Tools

This project contains tools for running Quorum clusters and integration testing Quorum.

At the moment, this project runs all Quorum clusters with Raft-based consensus by default and depends on [dynamic raft membership support in Quorum](https://github.com/jpmorganchase/quorum/pull/130). We'll be adding QuorumChain support in short order.

## Installation

First install Haskell [Stack](https://www.haskell.org/downloads#stack).

Now, in the project directory:

```
$ stack setup # this is only necessary to run once
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

To run all integration tests in batch, make sure that you can `sudo` (for packet filtering), and then run:

```
$ stack test
```

The following invocation might help to enable packet filtering during the test suite, if `sudo` requires a password on your machine:

```
$ sudo whoami && stack test
```

To run tests interactively, you can run them from the REPL:

```
$ stack ghci quorum-tools:lib
```

## Tests

Here are some high-level cluster tests that we include in our suite:

* Continually adding and removing nodes from a cluster until none of the initial members are left
* Partitioning a node from the rest of the network
* Public and [private state](https://github.com/jpmorganchase/quorum/wiki/Transaction-Processing) consistency
* Stopping, then restarting a node
* Revoking a node's membership in the cluster, re-registering it, and bringing it back online

The test sources are located in `src/QuorumTools/Test/`.

### Running a cluster

We also include scripts for running a cluster without necessarily testing it.

* `local-new`: create and start a new cluster, destroying old data directories (under `gdata` in the current directory)
* `local-start`: start a cluster from existing data directories (under `gdata` in the current directory)
* `local-spam`: send a rate-limited stream of transactions to a geth node

`local-new` runs indefinitely, with multiple `geth`s forked from the process. While the cluster is up and running, you can inspect the logs from the geth nodes (e.g. `tail -f geth1.log`), or send in transactions -- e.g. `local-spam -g 1 -r 10` will send 10 transactions per second to geth 1 while it is running. Additionally you can attach to a geth node via its IPC file under `gdata`: `geth attach gdata/geth1.geth.ipc`. If the `local-new` process is stopped, you can restart the cluster from the existing datadirs under `gdata` by issuing `local-start`.
