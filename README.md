gen_cluster
=====

Helper library for connecting nodes together through implementations of the
`gc_disovery` behaviour for discovering the nodes to connect to. `gc_dist` is a
behaviour for what is needed to connect the nodes (for example, to use
[partisan](https://github.com/lasp-lang/partisan) in place of built-in
distribution).

This is still in very early stages of development, but based off of past projects
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
