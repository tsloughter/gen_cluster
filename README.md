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

## Configuration

If configuration is given in `sys.config` for `gen_cluster` it will start on
boot. To start in your supervision tree simply leave `gen_cluster` out of
configuration or set the configuration to the empty list, `[]`.

The simplest way to try `gen_cluster` is with `gc_discover_epmd_hosts` which
will connect to all other hosts that connect to the same `epmd`. The empty
configuration is the same as `#{hosts => [localhost]}`:

```erlang
{gen_cluster, [{discovery, {gc_discover_epmd_hosts, #{}}}]}
```

or `gc_discover_static` which takes a list of nodes to connect to, for example:

```erlang
{gen_cluster, [{discovery, {gc_discover_static, ['a@rosa', 'b@rosa']}}]}
```

To disable reconnection set `refresh_interval_ms` to `infinity`.

An example of using DNS to connect nodes is found in `examples/`, the
`gen_cluster` configuration is in the example `k8s_erlang_cluster` is:

```erlang
{gen_cluster, [{discovery, {gc_discover_dns_a, #{domain => "k8s-erlang-cluster.k8s-erlang-cluster"}}}]},
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

### Starting Manually

If no configuration is given to `gen_cluster` on boot it will not start a
`gen_cluster` process. You can start `gen_cluster` with `start_link`:

```erlang
gen_cluster:start_link([{discovery, {gc_discover_dns_a, #{domain =>
                                                           "k8s-erlang-cluster.k8s-erlang-cluster"}}}])
```

Or in your in your supervision tree.
