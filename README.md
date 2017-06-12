# Quorum Tools

This repo contains tools for running a Quorum cluster and integration testing Quorum.

## Installation

First install Haskell [Stack](https://www.haskell.org/downloads#stack).

Now, in the project directory:

```
$ stack setup
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

To run tests interactively, load the REPL:

```
$ stack ghci quorum-tools:lib
```

## Tests

This repo tests the following cases:

* A network partition of one node in a running cluster (including the case where this node is the Raft leader)
* Public and private state consistency
* Completely stopping and restarting a node in a running cluster.
* etc

The test sources are located in `src/QuorumTools/Test/`.

### Running a cluster

We also include scripts for running a cluster without necessarily testing it.

* `local-new`: creat and start a new cluster, destroying old data directories
* `local-start`: start an existing cluster
* `local-spam`: send a rate-limited stream of transactions to a geth node
