-module(gc_discover_static).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-record(state, {peers :: gen_cluster:peers()}).

-spec init([node()]) -> {ok, #state{}}.
init(Nodes) ->
    {ok, #state{peers=gen_cluster:nodes_to_peers(Nodes)}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{peers=Peers}) ->
    Peers.

%%
