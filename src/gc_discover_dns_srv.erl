-module(gc_discover_dns_srv).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-record(state, {node_name   :: string(),
                srv         :: string(), %% unexported: inet_res:dns_name(),
                lookup_opts :: [inet_res:res_option()]}).

-spec init(#{domain      := string(), %% unexported: inet_res:dns_name(),
             lookup_opts => [inet_res:res_option()]}) -> {ok, #state{}}.
init(Opts=#{domain := Domain}) ->
    LookupOpts = maps:get(lookup_opts, Opts, []),
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    {ok, #state{node_name=NodeName,
                srv=Domain,
                lookup_opts=LookupOpts}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{node_name=NodeName,
             srv=Domain,
             lookup_opts=LookupOpts}) ->
    LookupResults = inet_res:lookup(Domain, in, srv, LookupOpts),
    %% the `is_list' is for making eqwalizer happy
    lists:foldl(fun({_, _, Port, Host}, PeersAcc) when is_list(Host) ->
                    Node = list_to_atom(string:join([NodeName, Host], "@")),
                    sets:add_element(#{node => Node,
                                       port => Port}, PeersAcc)
                end, sets:new([{version, 2}]), LookupResults).

%%
