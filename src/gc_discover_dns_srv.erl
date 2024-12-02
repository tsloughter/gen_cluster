-module(gc_discover_dns_srv).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-type host_type() :: ip | hostname.
-type dns_name()  :: string(). %% unexported: inet_res:dns_name()

-record(state, {node_name      :: string(),
                srv            :: dns_name(),
                host_type      :: host_type(),
                lookup_timeout :: erlang:timeout()}).

-spec init(#{domain      := string(), %% unexported: inet_res:dns_name(),
             host_type      => host_type(),
             lookup_opts => [inet_res:res_option()]}) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    Timeout = maps:get(lookup_timeout, Opts, 5000),
    HostType = maps:get(host_type, Opts, hostname),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                srv=Domain,
                host_type=HostType,
                lookup_timeout=Timeout}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             srv=Domain,
             host_type=HostType,
             lookup_timeout=Timeout}) ->
    Set = sets:new([{version, 2}]),
    case inet_res:getbyname(Domain, srv, Timeout) of
        {ok, {hostent, _, _, _, _, Hosts}} ->
            %% the `is_list' is for making eqwalizer happy
            lists:foldl(fun({_, _, Port, Host}, PeersAcc) when is_list(Host) ->
                                handle_host(Host, Port, NodeName, HostType, PeersAcc)
                        end, Set, Hosts);
        {error, _} ->
            Set
    end.

%%

-spec handle_host(dns_name(), integer(), dns_name(), host_type(), gen_cluster:peers())
                 -> gen_cluster:peers().
handle_host(Host, Port, NodeName, HostType, Peers) ->
    case parse_host(Host, HostType) of
        {ok, Host} ->
            %% elp:ignore W0023
            Node = list_to_atom(string:join([NodeName, Host], "@")),
            sets:add_element(#{node => Node,
                               port => Port}, Peers);
        {error, _} ->
            %% TODO: log a debug/info/warning
            Peers
    end.

-spec parse_host(dns_name(), host_type()) -> {ok, string()} | {error, term()}.
parse_host(Host, ip) ->
    case inet_res:getbyname(Host, a, 5000) of
        {error, einval} ->
            {error, einval};
        {ok, {hostent, _, _, _, _, [IP={_, _, _, _}]}} ->
            parse_ip(IP);
        {ok, {hostent, _, _, _, _, [IP={_, _, _, _, _, _, _, _}]}} ->
            parse_ip(IP)
    end;
parse_host(Host, hostname) ->
    {ok, Host}.

parse_ip(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            {error, einval};
        String ->
            {ok, String}
    end.
