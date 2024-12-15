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

* Erlang/OTP 27+ (I wanted to try the new docs)

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

See documentation in the examples or on Hex Docs see the sidebar for the
Kubernetes section.

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

# Testing

Right now tests are all done on the DNS method of discovery and based on the
examples. The setup and configuration is only done in GitHub Actions so the
quickest way to run them locally would be with [`act`](https://nektosact.com/).

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
