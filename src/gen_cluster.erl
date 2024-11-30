%%%-------------------------------------------------------------------
%% @doc state machine to manage state of the cluster.
%% @end
%%%-------------------------------------------------------------------
-module(gen_cluster).

-behaviour(gen_statem).

-export([start_link/0]).

-export([init/1,
         active/3,
         inactive/3,
         callback_mode/0,
         terminate/3,
         code_change/4]).

-export([nodes_to_peers/1]).

%% uses default port if none given
%% -type node_address() :: {inet:ip_address(), inet:port_number()} |
%%                         {string(), inet:port_number()} |
%%                         inet:ip_address() |
%%                         string().

-type option() :: {discover, module()} |
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

%% type             :: {static, [node_address()]}
%%                                  | {fqdns, string()}
%%                                  | {srv, string()}
%%                                  | undefined,
%%                seen                :: sets:set(),
-record(data, {discovery           :: {module(), cb_state()},
               dist                :: {module(), cb_state()},
               refresh_interval_ms :: integer() | infinity}).

-define(SERVER, ?MODULE).
-define(DEFAULT_REFRESH_INTERVAL_MS, 5000).

start_link() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() ->
    [state_functions].

init([]) ->
    case application:get_all_env(gen_cluster) of
        [] ->
            ignore;
        Configuration ->
            Data = data_from_config(Configuration),
            {ok, inactive, Data, [{next_event, internal, refresh}]}
    end.

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

-spec data_from_config([{atom(), dynamic()}]) -> #data{}.
data_from_config(Configuration) ->
    Discovery = init_callback(proplists:get_value(discovery,
                                                  Configuration,
                                                  {gc_discover_static, []})),
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

init_callback({Module, Config}) ->
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
