-module(gc_discover_dns_ip).
-moduledoc "
Discovery method that takes a host name to query DNS A records for and
uses all the returned hosts as peers to connect or reconnect to. Whether
to use the IP or the hostname associated with each IP result is configured with
the option `host_type` which takes `ip` or `hostname` as values. The default
is `ip` which uses the IP as is in the node name. When it
is `hostname` each returned IP is passed to `inet_res:gethostbyaddr/1`
to get a hostname for the address.

Since this is a built-in discovery module it has an alias, `dns`, which
can be used in place of the full module name, `gc_discovery_dns_ip`. The
alias `dns` covers both `gc_discover_dns_ip` and `gc_discover_dns_srv`.
This method (`gc_discover_dns_ip`) is used by default and when the option
`record_type => ip` is given.

The name part of the node name to connect to is taken from the current node's name.

The following is an example relying on the alias, `dns`, and that `record_type`
defaults to `ip`:

```
{gen_cluster, [{discovery, {dns, #{domain => \"k8s-erlang-cluster.k8s-erlang-cluster\"}}}]}
```

This results in the A records for `k8s-erlang-cluster.k8s-erlang-cluster`
being queried for on every `gen_cluster` refresh interval and it tries to
establish connections to those not already connected to.

To use IPv6 set `ipv6` to `true`:

```
{gen_cluster, [{discovery, {dns, #{domain => \"k8s-erlang-cluster.k8s-erlang-cluster\",
                                   ipv6 => true}}}]}
```

This results in the AAAA records of `k8s-erlang-cluster.k8s-erlang-cluster`
being queried for.
".

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-doc "
Module options. Takes a domain to query for, an optional `host_type` which
can be `ip` or `hostname`, a boolean `ipv6` for whether to query for `A` or
`AAAA` records and `lookup_timeout` which is the time each DNS query can take.
".
-type options() :: #{domain         := gc_discover_dns:dns_name(),
                     host_type      => gc_discover_dns:host_type(),
                     ipv6           => boolean(),
                     lookup_timeout => erlang:timeout()}.

-export_type([options/0]).

-record(state, {node_name      :: string(),
                domain         :: gc_discover_dns:dns_name(),
                host_type      :: gc_discover_dns:host_type(),
                ip_record_type :: gc_discover_dns:ip_record_type(),
                lookup_timeout :: erlang:timeout()}).

-include_lib("kernel/include/inet.hrl").

-spec init(options()) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    Timeout = maps:get(lookup_timeout, Opts, 5000),
    HostType = maps:get(host_type, Opts, ip),
    UseIPv6 = maps:get(ipv6, Opts, false),
    IPRecordType = gc_discover_dns:ip_record_type(UseIPv6),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                domain=Domain,
                host_type=HostType,
                ip_record_type=IPRecordType,
                lookup_timeout=Timeout}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             domain=Domain,
             host_type=HostType,
             ip_record_type=IPRecordType,
             lookup_timeout=Timeout}) ->
    Set = sets:new([{version, 2}]),
    case inet_res:getbyname(Domain, IPRecordType, Timeout) of
        {ok, #hostent{h_addr_list=IPs}} ->
            lists:foldl(fun(IP, PeersAcc) ->
                                handle_ip(IP, NodeName, HostType, PeersAcc)
                        end, Set, IPs);
        {error, _} ->
            Set
    end.

%%

-spec handle_ip(inet:ip_address(), gc_discover_dns:dns_name(), gc_discover_dns:host_type(), gen_cluster:peers())
               -> gen_cluster:peers().
handle_ip(IP, NodeName, HostType, PeersAcc) ->
    case parse_ip(IP, HostType) of
        {ok, Host} ->
            %% elp:ignore W0023
            Node = list_to_atom(string:join([NodeName, Host], "@")),
            sets:add_element(#{node => Node}, PeersAcc);
        {error, _} ->
            %% TODO: log a debug/info/warning
            PeersAcc
    end.

-spec parse_ip(inet:ip_address(), gc_discover_dns:host_type()) -> {ok, string()} | {error, term()}.
parse_ip(IP, HostType) when HostType =:= ip orelse HostType =:= ipv6 ->
    case inet:ntoa(IP) of
        {error, einval} ->
            {error, einval};
        String ->
            {ok, String}
    end;
parse_ip(IP, hostname) ->
    case inet_res:gethostbyaddr(IP) of
        {ok, #hostent{h_name=Host}} ->
            {ok, to_string(Host)};
        {error, _}=Error ->
            Error
    end.

to_string(Host) when is_atom(Host) ->
    atom_to_list(Host);
to_string(Host) when is_list(Host) ->
    Host.
