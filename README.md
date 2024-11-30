gen_cluster
=====

Helper library for connecting nodes together.

## Configuration

```erlang
[
 {gen_cluster, [{discovery, {gc_discover_static, ['a@rosa', 'b@rosa']}},
                {dist, {gc_dist_erl, []}},
                {refresh_interval_ms, 5000}]
].
```

