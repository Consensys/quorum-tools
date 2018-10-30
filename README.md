A CLI (`qctl`) provides a set of tools for Quorum Network management (using [Docker](https://www.docker.com/)):
* Bring up a simple Quorum Network
* Bring up a managed Quorum Network by exposing additional APIs: add new Quorum nodes, start/stop existing Quorum nodes
* Istanbul BFT utilities: encode/decode `extraData`


[Getting started](#getting-started)

[How to](#how-to)

[Quorum Network Configuration](#quorum-network-configuration)

[Operator APIs](#operator-apis)
  * [`GET /v1/nodes`](#get-v1nodes) - retrieve nodes
  * [`PUT /v1/nodes`](#put-v1nodes) - create new nodes to existing network. This only bring up new nodes and configure permissioned/discovery. Other actions like proposing Istanbul validators or add RAFT peers must be done seperately via JSON RPC
  * [`POST /v1/nodes/{idx}`](#post-v1nodesidx) - perform an action on a node
  * [`GET /v1/nodes/{idx}/{target}/logs`](#get-v1nodesidxtargetlogs) - Websocket endpoint to stream logs from a node

[Development](#development)

### Getting started

* Download cross-compiled binaries
* Build from source
  * Require Go 1.11.x (Go modules are used so unset `GOPATH`) and `make`
  * Clone this repo and run `make`, binary will be available under `build/` folder

### How to

See [docs/qctl.md](docs/qctl.md) for details of different commands

### Quorum Network Configuration

See [samples](samples/)

### Operator APIs

When bringin up a managed Quorum Network with `--enable-operator` flag, the following APIs are exposed

#### `GET /v1/nodes`
**Response:**
```json
[
    {
        "id": "0",
        "url": "http://localhost:22000",
        "privacy-address": "xyz=",
        "validator-address": "0xaf234abcd"
    },
    ...
]
```
If `Accept` header has value `application/yaml`, the response would be
```yaml
quorum:
  nodes:
    Node1:
      privacy-address: BULeR8JyUWhiuuCMU/HLA0Q5pzkYT+cHII3ZKBey3Bo=
      url: http://localhost:22000
    Node2:
      privacy-address: QfeDAys9MPDs2XHExtc84jKGHxZg/aj52DTh0vtA3Xc=
      url: http://localhost:22001
```

#### `PUT /v1/nodes`

**Request:**
```json
[
	{
    	"quorum": {
            "image": "quorumengineering/quorum:latest",
            "config": {
                "verbosity" : "5"
            }
        },
        "tx_manager": {
            "image": "quorumengineering/tessera:latest",
            "config": {
            }
        }
	},
	...
]
```

**Response:**
```json
[
    {
        "id": "7",
        "url": "http://localhost:22000",
        "privacy-address": "xyz=",
        "validator-address": "0xaf234abcd"
    },
    ...
]
```

#### `POST /v1/nodes/{idx}`

**Request:**
```json
{
  "target": "quorum/tx_manager",
  "action": "stop/start/restart"
}
```

#### `GET /v1/nodes/{idx}/{target}/logs`

Logs are streamed to response body

### Development

* Go 1.11.x
* Unset `GOPATH`
