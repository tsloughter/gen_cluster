gen_cluster
=====

[![Tests](https://github.com/tsloughter/gen_cluster/actions/workflows/ci.yml/badge.svg)](https://github.com/tsloughter/gen_cluster/actions/workflows/ci.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/gen_cluster.svg?style=flat)](https://hex.pm/packages/gen_cluster)
[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/gen_cluster/)

`gen_cluster` is a helper library for connecting and reconnecting nodes together
through implementations of the `gc_disovery` behaviour for discovering the nodes
to connect to. `gc_dist` is a behaviour for what is needed to connect the nodes
(for example, to use [partisan](https://github.com/lasp-lang/partisan) in place
of built-in distribution).

This is still in very early stages of development. It is based off of past projects
that did the same thing but builtin and redone every time I worked on a new
project somewhere.

The API may (will) change! This is a first pass at a generalization.

# Requirements

* Erlang/OTP 27+

# Usage

By default no clustering is done when `gen_cluster` is started. If `gen_cluster`
is started on boot of an OTP release or with
`application:ensure_all_started(gen_cluster)` and the application environment is
`[]` or not set at all then no `gen_cluster` process is started, only the
`supervisor` process is started with an empty child list. Then you can instead
start `gen_cluster` in your own supervision tree with `gen_cluster:start_link`.

If there is configuration in the application environment then a `gen_cluster`
process is started by the top level supervisor and it attempts to connect to all
nodes returned by the configured discovery module (the default is `{static, []}`
which returns no nodes) and it looks for new or disconnected nodes on an
interval (defaults to `5000` milliseconds).

Even if started on boot additional clusters can be joined with
`gen_cluster:start_link` (as in, with a separate distribution module than Erlang
distribution you could connect to an additional cluster).

# Configuration

## Application Environment

### EPMD and Static

If configuration is given in `sys.config` for `gen_cluster` it will start on
boot. 

The simplest way to try `gen_cluster` is with `gc_discover_epmd_all` which
will connect to all other hosts that connect to the same `epmd`. The empty
configuration is the same as `#{hosts => [localhost]}`:

```erlang
{gen_cluster, [{discovery, {epmd_all, #{}}}]}
```

or `gc_discover_static` which takes a list of nodes to connect to, for example
on my system `rosa` with 2 nodes `a` and `b` already running:

```erlang
{gen_cluster, [{discovery, {static, ['a@rosa', 'b@rosa']}}]}
```

### DNS

To disable reconnection set `refresh_interval_ms` to `infinity`.

An example of using DNS to connect nodes is found in `examples/`, the
`gen_cluster` configuration is in the example `k8s_erlang_cluster` is:

```erlang
{gen_cluster, [{discovery, {dns, #{domain => "k8s-erlang-cluster.k8s-erlang-cluster"}}}]},
```

Configure `refresh_interval_ms` (default `5000`) to have `gen_cluster` look for
new, disconnected or removed nodes more or less often.

Currently a node removed will not be disconnected but if its connection goes
down and it is not still in the list of discovered nodes it won't attempt a
reconnect. How this works in the future is undecided. It will likely be
configurable and default to not disconnecting nodes still in the node list but
not in the discovery list, this is to allow for graceful shutdown in the case a
node is shutting down, but still completing work, and gets removed from DNS in
order to not get new work but may still need to communicate its final results or
handoff.

Other future configuration changes are the reconnect interval being separate
from refresh with a backoff tracked per-node it is attempting to reconnected.

#### IPv6

Set `ipv6` to true in the configuration map to tell `gen_cluster` to use IPv6
for either the initial DNS query -- used when the `gc_discover_dns_ip` module is
used -- or for turning returned domains into IP's -- used when a `SRV` query is
used with `host_type` set to `ip` to turn the returned domains into IPs.

## Starting Manually

If no configuration is given to `gen_cluster` on boot it will not start a
`gen_cluster` process. You can start `gen_cluster` with `start_link`:

```erlang
gen_cluster:start_link([{discovery, {dns, #{domain =>
                                           "k8s-erlang-cluster.k8s-erlang-cluster"}}}])
```

Or as a child in your supervision tree:

```erlang
GenClusterConfig = [{discovery, {ip, #{domain =>
                                       "k8s-erlang-cluster.k8s-erlang-cluster"}}}],
[#{id => gen_cluster,
  start => {gen_cluster, start_link, [GenClusterConfig]}}]
```

# Using in Kubernetes

## Setup

Under `examples/k8s_erlang_cluster` there is an example project that builds an
Erlang release with [kustomize](https://kustomize.io/) configuration to deploy a
cluster to Kubernetes. The example's `config/sys.config` contains configuration
for `gen_cluster` which is started on boot of the release:

```erlang
{discovery, {dns, #{domain => "k8s-erlang-cluster.k8s-erlang-cluster"}}}
```

This tells `gen_cluster` to use the DNS module for finding peers which defaults
to looking up `A` records and using the IPs as the host part of each node name.
The `domain` is set to `<service>.<namespace>` which resolves to records for
the service `k8s-erlang-cluster` in a namespace `k8s-erlang-cluster`.

To set the node name properly there are 3 pieces of configuration. First,
`config/vm.args.src` sets `-name` to use an environment variable `POD_IP`:

```shell
-name k8s_erlang_cluster@${POD_IP}
```

Next, the environment of the container that runs the release is made to set the
environment variable `POD_IP` to the IP address of that Pod in
`deployment/base/deployment.yaml`:

```yaml
env:
- name: ERL_DIST_PORT
  value: "39135"
- name: POD_IP
  valueFrom:
    fieldRef:
        fieldPath: status.podIP
```

Note that `ERL_DIST_PORT` is also set. This tells the VM what port to listen for
distribution connections on, as well as what port to connect to other nodes on.
Setting the variable will disable EPMD as it is not needed if we know the port
to use and the port used by all other nodes in the cluster.

Lastly, a [headless
service](https://kubernetes.io/docs/concepts/services-networking/service/#headless-services)
is used to expose the IPs of these Pods through DNS. A headless service is
created when `clusterIP` is set to `None` in the `Service` resource, found in
`deployment/base/service.yaml`:

```yaml
clusterIP: None
ports:
- protocol: TCP
  port: 39135
  targetPort: 39135
  name: dist-erl
```

A Service without `clusterIP: None` would have a single IP to load balance
requests to the Pods through. The headless service results in an Endpoint
per-Pod, see below in the steps to setup the cluster in
[kind](https://kind.sigs.k8s.io/) for an example.

## Run Example

The example can be installed to any Kubernetes cluster but comes with a
configuration (`examples/k8s_erlang_cluster/cluster.yaml`) for creating the
cluster with [kind](https://kind.sigs.k8s.io/) via the tool
[ctlptl](https://github.com/tilt-dev/ctlptl):

```shell
$ ctlptl apply -f cluster.yaml
```

The `cluster.yaml` configuration creates a kind cluster with a Docker registry
on localhost port 5005. This allows for the publishing of a Docker image for the
example release locally in a place the kind cluster can pull from:

```shell
$ docker build -t localhost:5005/k8s_erlang_cluster:0.1.0
$ docker push localhost:5005/k8s_erlang_cluster:0.1.0
```

The [kustomize](https://kustomize.io/) configuration can be used to install the
release with `kubectl apply -k`:

```shell
$ kubectl apply -k deployment/base
namespace/k8s-erlang-cluster created
configmap/k8s-erlang-cluster created
service/k8s-erlang-cluster created
deployment.apps/k8s-erlang-cluster created
```

With `get endpoints` we can view the results of the creation of a headless
Service that matches 3 Pods:

```shell
$ kubectl -n k8s-erlang-cluster get endpoints k8s-erlang-cluster
NAME                 ENDPOINTS                                            AGE
k8s-erlang-cluster   10.244.0.5:39135,10.244.0.6:39135,10.244.0.7:39135   7m11s
```

To see that the cluster is formed run a command against a single Pod in the
Deployment with `exec`:

```shell
$ kubectl exec -n k8s-erlang-cluster deploy/k8s-erlang-cluster -- bin/k8s_erlang_cluster eval 'nodes().'
['k8s_erlang_cluster@10.244.0.5', 'k8s_erlang_cluster@10.244.0.7']
```

# Implementing Third Party Discovery or Connection

There is a behaviour for both discovery (`gc_discover`) and peer connection
(`gc_dist`) respectively. Discover must have an `init/1` for initializing and
returning any needed state for later returning a list of peers to be connected
to:

```erlang
-callback init(term()) -> {ok, cb_state()}.
-callback peers(cb_state()) -> gen_cluster:peers().
```

The discover module must have an `init/1` for initializing and returning any
necessary state, the `members` callback which is for returning the current list of
connected peers in the cluster, and a callback for both connecting and
disconnecting from peers:

```erlang
-callback init(cb_state()) -> {ok, cb_state()}.
-callback members(cb_state()) -> gen_cluster:peers().
-callback connect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.
-callback disconnect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.
```

# Acknowledgments

More projects than I can recount that I've worked on has simply redone this
logic. Usually this was before K8s. Then in
[Erleans](https://github.com/erleans/erleans) I had to do it for k8s and local
and redid the logic again. As I start to break things out of Erleans and
simplify it this is one of the first parts to become its own library, but there
is a long history before Erleans. Long ago I thought
[Erlware](https://github.com/erlware/) had an app but I can't find it on Google
Code, only [nodefinder](https://code.google.com/archive/p/nodefinder/) from
[Dukes of Erl](http://dukesoferl.blogspot.com/).

[nodefinder](https://github.com/okeuday/nodefinder/tree/master/src) by
[okeuday](https://github.com/okeuday/) is a successor to the that project. It
has support for EC2 and multi-cast.

In [Elixir](https://elixir-lang.org/) everyone uses
[libcluster](https://github.com/bitwalker/libcluster/).

And in [Gleam](https://gleam.run/) the library
[Barnacle](https://github.com/Pevensie/barnacle) is the way to go.

`gen_cluster` takes inspiration from all these works.
