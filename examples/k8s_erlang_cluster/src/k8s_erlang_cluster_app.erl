%%%-------------------------------------------------------------------
%% @doc k8s_erlang_cluster public API
%% @end
%%%-------------------------------------------------------------------

-module(k8s_erlang_cluster_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    k8s_erlang_cluster_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
