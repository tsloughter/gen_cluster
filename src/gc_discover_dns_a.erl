-module(gc_discover_dns_a).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-type host_type() :: ip | hostname.
-type dns_name()  :: string(). %% unexported: inet_res:dns_name()

-record(state, {node_name      :: string(),
                domain         :: dns_name(),
                host_type      :: host_type(),
                lookup_timeout :: erlang:timeout()}).

-include_lib("kernel/include/inet.hrl").

-spec init(#{domain         := dns_name(),
             host_type      => host_type(),
             lookup_timeout => erlang:timeout()}) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    Timeout = maps:get(lookup_timeout, Opts, 5000),
    HostType = maps:get(host_type, Opts, ip),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                domain=Domain,
                host_type=HostType,
                lookup_timeout=Timeout}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             domain=Domain,
             host_type=HostType,
             lookup_timeout=Timeout}) ->
    Set = sets:new([{version, 2}]),
    case inet_res:getbyname(Domain, a, Timeout) of
        {ok, #hostent{h_addr_list=IPs}} ->
            lists:foldl(fun(IP, PeersAcc) ->
                                handle_ip(IP, NodeName, HostType, PeersAcc)
                        end, Set, IPs);
        {error, _} ->
            Set
    end.

%%

-spec handle_ip(inet:ip_address(), dns_name(), host_type(), gen_cluster:peers())
               -> gen_cluster:peers().
handle_ip(IP, NodeName, HostType, PeersAcc) ->
    {ok, Host} = parse_record(IP, HostType),
    Node = list_to_atom(string:join([NodeName, Host], "@")),
    sets:add_element(#{node => Node}, PeersAcc).

-spec parse_record(inet:ip_address(), host_type()) -> {ok, string()} | {error, term()}.
parse_record(Ip, ip) ->
    case inet:ntoa(Ip) of
        {error, einval} ->
            {error, einval};
        String ->
            {ok, String}
    end;
parse_record(Ip, hostname) ->
    {ok, {hostent, Host, _, _, _, _}} = inet_res:gethostbyaddr(Ip),
    {ok, to_string(Host)}.

to_string(Host) when is_atom(Host) ->
    atom_to_list(Host);
to_string(Host) when is_list(Host) ->
    Host.
