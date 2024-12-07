-module(gen_cluster).
-moduledoc "
Process that manages calling the necessary discovery and connect/disconnect
functions at a configurable interval.
".

-behaviour(gen_statem).

-export([start_link/1]).

-export([init/1,
         active/3,
         inactive/3,
         callback_mode/0,
         terminate/3,
         code_change/4]).

-export([nodes_to_peers/1]).

-type builtin_discovery() :: {dns, gc_discover_dns_ip:options() | gc_discover_dns_srv:options()} |
                             {static, gc_discover_static:options()} |
                             {epmd_all, gc_discover_epmd_all:options()}.

-type option() :: {discovery,
                   builtin_discovery() |
                   {module(), term()}} |
                  {dist, module()} |
                  {refresh_interval_ms, integer() | undefined}.

-type config() :: [option()].

-type peer()   :: #{node := atom(),
                    port => inet:port_number()}.

-type peers() :: sets:set(peer()).

%%

-type cb_state() :: term().

-export_type([config/0,
              peer/0,
              peers/0]).

-record(data, {discovery           :: {module(), cb_state()},
               dist                :: {module(), cb_state()},
               refresh_interval_ms :: integer() | infinity}).

-define(DEFAULT_REFRESH_INTERVAL_MS, 5000).

-spec start_link(config()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Configuration) ->
    gen_statem:start_link(?MODULE, [Configuration], []).

callback_mode() ->
    [state_functions].

-spec init([config()]) -> gen_statem:init_result(inactive | active, #data{}).
init([Configuration]) ->
    Data = data_from_config(Configuration),
    {ok, inactive, Data, [{next_event, internal, refresh}]}.

inactive(internal, refresh, Data=#data{refresh_interval_ms=RefreshIntervalMs}) ->
    Data1 = handle_refresh(Data),
    {next_state, active, Data1, [{timeout, RefreshIntervalMs, refresh}]};
inactive(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

active(timeout, refresh, Data=#data{refresh_interval_ms=RefreshIntervalMs}) ->
    Data1 = handle_refresh(Data),
    {keep_state, Data1, [{timeout, RefreshIntervalMs, refresh}]};
active(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_, _OldState, Data, _) ->
    {ok, Data}.

%% Internal functions

-spec data_from_config(config()) -> #data{}.
data_from_config(Configuration) ->
    Discovery = init_callback(proplists:get_value(discovery,
                                                  Configuration,
                                                  {static, []})),
    Dist = init_callback(proplists:get_value(dist,
                                             Configuration,
                                             {gc_dist_erl, []})),
    RefreshIntervalMs =
        proplists:get_value(refresh_interval_ms,
                            Configuration,
                            ?DEFAULT_REFRESH_INTERVAL_MS),
    #data{discovery=Discovery,
          dist=Dist,
          refresh_interval_ms=RefreshIntervalMs}.

init_callback({dns, Config}) ->
    Module = to_dns_module(maps:get(record_type, Config, ip)),
    init_callback_({Module, Config});
init_callback({static, Config}) ->
    init_callback_({gc_discover_static, Config});
init_callback({epmd_all, Config}) ->
    init_callback_({gc_discover_epmd_all, Config});
init_callback({Module, Config}) ->
    %% use a separate function here so the `dns' case can't accidentally cause an
    %% infinite loop if `record_type' were set to `dns'
    init_callback_({Module, Config}).

init_callback_({Module, Config}) ->
    {ok, State} = run_callback({Module, Config}, init, []),
    {Module, State}.

handle_refresh(Data=#data{discovery=Discovery,
                          dist=Dist}) ->
    Members = run_callback(Dist, members, []),
    FoundPeers = run_callback(Discovery, peers, []),
    ToConnect = sets:subtract(FoundPeers, Members),
    _ = sets:fold(fun(Peer, _) ->
                          maybe_connect(Peer, Members, Dist)
                  end, [], ToConnect),
    Data.

maybe_connect(Peer, Members, Dist) ->
    case sets:is_element(Peer, Members) of
        false ->
            run_callback(Dist, connect, [Peer]);
        true ->
            false
    end.

-spec nodes_to_peers([node()]) -> peers().
nodes_to_peers(Nodes) ->
    lists:foldl(fun(Name, Acc) ->
                      sets:add_element(#{node => Name}, Acc)
                end, sets:new([{version, 2}]), Nodes).

-spec run_callback({module(), cb_state()}, atom(), list()) -> dynamic().
run_callback({CallbackMod, State}, Fun, Args) ->
    erlang:apply(CallbackMod, Fun, Args ++ [State]).

to_dns_module(ip) ->
    gc_discover_dns_a;
to_dns_module(srv) ->
    gc_discover_dns_srv.
