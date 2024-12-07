-module(gc_discover_dns_srv).
-moduledoc "
Discovery method that takes a host name to query DNS SRV records for and
uses all the returned hosts as peers to connect or reconnect to. Whether
to use the hostname or IP for each hostname result is configured with
the option `host_type` which takes `hostname` or `ip` as values. The default
is `hostname` which keeps the result as is in the node name. When it
is `ip` each returned hostname is passed to `inet_res:getbyname/3`
to get an IP.

Since this is a built-in discovery module it has an alias, `dns`, which
can be used in place of the full module name, `gc_discovery_dns_srv`. The
alias `dns` covers both `gc_discover_dns_ip` and `gc_discover_dns_srv`.
This module (`gc_discover_dns_srv`) is used when the option
`record_type => srv` is given.

The name part of the node name to connect to is taken from the current node's name.

The following is an example relying on the alias, `dns`:

```
{gen_cluster, [{discovery, {dns, #{record_type => srv,
                                   domain => \"k8s-erlang-cluster.k8s-erlang-cluster\"}}}]}
```

This results in the SRV records for `k8s-erlang-cluster.k8s-erlang-cluster`
being queried for every `gen_cluster` refresh interval and it tries to
establish connections to those not already connected to.

To use IPv6 addresses as the host part of each node name set both
`host_type` to `ip` and `ipv6` to `true`:

```
{gen_cluster, [{discovery, {dns, #{record_type => srv,
                                   domain => \"k8s-erlang-cluster.k8s-erlang-cluster\",
                                   host_type => ip,
                                   ipv6 => true}}}]}
```
".

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-doc "
Module options. Takes `domain`, the domain to query for, an optional `host_type` which
can be `ip` or `hostname`, the optional boolean `ipv6` for whether to query for `A` or
`AAAA` record when `host_type` is `ip` and optional `lookup_timeout` which is the time
each DNS query can take (defaults to 5000 milliseconds).
".
-type options() :: #{domain      := string(), %% unexported: inet_res:dns_name(),
                     host_type   => gc_discover_dns:host_type(),
                     ipv6        => boolean(),
                     lookup_opts => [inet_res:res_option()]}.

-export_type([options/0]).

-record(state, {node_name      :: string(),
                srv            :: gc_discover_dns:dns_name(),
                host_type      :: gc_discover_dns:host_type(),
                ip_record_type :: gc_discover_dns:ip_record_type(),
                lookup_timeout :: erlang:timeout()}).

-spec init(options()) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    Timeout = maps:get(lookup_timeout, Opts, 5000),
    HostType = maps:get(host_type, Opts, hostname),
    UseIPv6 = maps:get(ipv6, Opts, false),
    RecordType = gc_discover_dns:ip_record_type(UseIPv6),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                srv=Domain,
                host_type=HostType,
                ip_record_type=RecordType,
                lookup_timeout=Timeout}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             srv=Domain,
             host_type=HostType,
             ip_record_type=IPRecordType,
             lookup_timeout=Timeout}) ->
    Set = sets:new([{version, 2}]),
    case inet_res:getbyname(Domain, srv, Timeout) of
        {ok, {hostent, _, _, _, _, Hosts}} ->
            %% the `is_list' is for making eqwalizer happy
            lists:foldl(fun({_, _, Port, Host}, PeersAcc) when is_list(Host) ->
                                handle_host(Host, Port, IPRecordType, NodeName, HostType, PeersAcc)
                        end, Set, Hosts);
        {error, _} ->
            Set
    end.

%%

-spec handle_host(gc_discover_dns:dns_name(), integer(), gc_discover_dns:ip_record_type(),
                  gc_discover_dns:dns_name(), gc_discover_dns:host_type(), gen_cluster:peers())
                 -> gen_cluster:peers().
handle_host(Host, Port, IPRecordType, NodeName, HostType, Peers) ->
    case parse_host(Host, IPRecordType, HostType) of
        {ok, Host} ->
            %% elp:ignore W0023
            Node = list_to_atom(string:join([NodeName, Host], "@")),
            sets:add_element(#{node => Node,
                               port => Port}, Peers);
        {error, _} ->
            %% TODO: log a debug/info/warning
            Peers
    end.

-spec parse_host(gc_discover_dns:dns_name(), gc_discover_dns:ip_record_type(), gc_discover_dns:host_type())
                -> {ok, string()} | {error, term()}.
parse_host(Host, IPRecordType, ip) ->
    case inet_res:getbyname(Host, IPRecordType, 5000) of
        {error, einval} ->
            {error, einval};
        {ok, {hostent, _, _, _, _, [IP={_, _, _, _}]}} ->
            parse_ip(IP);
        {ok, {hostent, _, _, _, _, [IP={_, _, _, _, _, _, _, _}]}} ->
            parse_ip(IP)
    end;
parse_host(Host, _, hostname) ->
    {ok, Host}.

parse_ip(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            {error, einval};
        String ->
            {ok, String}
    end.
