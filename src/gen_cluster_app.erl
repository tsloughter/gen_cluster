%%%-------------------------------------------------------------------
%% @doc gen_cluster public API
%% @end
%%%-------------------------------------------------------------------

-module(gen_cluster_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gen_cluster_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
