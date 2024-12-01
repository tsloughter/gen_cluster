-module(gc_discover_dns_a).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-type host_type() :: ip | hostname.
-type dns_name()  :: string(). %% unexported: inet_res:dns_name()

-record(state, {node_name   :: string(),
                domain      :: dns_name(),
                host_type   :: host_type(),
                lookup_opts :: [inet_res:res_option()]}).

-spec init(#{domain      := dns_name(),
             host_type   => host_type(),
             lookup_opts => [inet_res:res_option()]}) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    LookupOpts = maps:get(lookup_opts, Opts, []),
    HostType = maps:get(host_type, Opts, ip),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                domain=Domain,
                host_type=HostType,
                lookup_opts=LookupOpts}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             domain=Domain,
             host_type=HostType,
             lookup_opts=LookupOpts}) ->
    LookupResults = inet_res:lookup(Domain, in, a, LookupOpts),
    %% the `is_list' is for making eqwalizer happy
    lists:foldl(fun(Ip={A, B, C, D}, PeersAcc) when is_integer(A) ,
                                                 is_integer(B) ,
                                                 is_integer(C) ,
                                                 is_integer(D) ->
                        handle_ip(Ip, NodeName, HostType, PeersAcc);
                   (Ip={A, B, C, D, E, F, G, H}, PeersAcc) when is_integer(A) ,
                                                   is_integer(B) ,
                                                   is_integer(C) ,
                                                   is_integer(D) ,
                                                   is_integer(E) ,
                                                   is_integer(F) ,
                                                   is_integer(G) ,
                                                   is_integer(H)  ->
                        handle_ip(Ip, NodeName, HostType, PeersAcc)
                end, sets:new([{version, 2}]), LookupResults).

%%

handle_ip(Ip, NodeName, HostType, PeersAcc) ->
    {ok, Host} = parse_record(Ip, HostType),
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
