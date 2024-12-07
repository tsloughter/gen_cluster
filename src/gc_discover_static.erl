-module(gc_discover_static).
-moduledoc "
Discovery method that takes a list of hosts to connect to and to
reconnect to if a connect is lost. Reconnection can be disabled by
including the option `refresh_interval_ms => infinity` when starting
`gen_cluster`.

This is a built-in discover implementation so has a short name,
`static`, as well.
".

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-doc "The options for starting this discover module, a list of node names".
-type options() :: [node()].

-export_type([options/0]).

-record(state, {peers :: gen_cluster:peers()}).

-spec init(options()) -> {ok, #state{}}.
init(Nodes) ->
    {ok, #state{peers=gen_cluster:nodes_to_peers(Nodes)}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{peers=Peers}) ->
    Peers.

%%
