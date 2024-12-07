-module(gen_cluster_app).
-moduledoc "
Application module which runs the top level `gen_cluster_sup` when started.
".
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gen_cluster_sup:start_link().

stop(_State) ->
    ok.

%%
