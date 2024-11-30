-module(gc_dist_erl).

-export([init/1,
         members/1,
         connect/2,
         disconnect/2]).

-record(state, {}).

-spec init(term()) -> {ok, #state{}}.
init(_) ->
    {ok, #state{}}.

-spec members(#state{}) -> gen_cluster:peers().
members(_) ->
    gen_cluster:nodes_to_peers(nodes()).

-spec connect(gen_cluster:peer(), #state{}) -> boolean() | ignored.
connect(#{node := Node}, _) ->
    net_kernel:connect_node(Node).

-spec disconnect(gen_cluster:peer(), #state{}) -> boolean() | ignored.
disconnect(#{node := Node}, _) ->
    erlang:disconnect_node(Node).
