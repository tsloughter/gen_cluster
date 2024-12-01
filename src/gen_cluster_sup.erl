%%%-------------------------------------------------------------------
%% @doc gen_cluster top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gen_cluster_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    ChildSpecs = case application:get_all_env(gen_cluster) of
                     [] ->
                         [];
                     Config ->
                         [#{id => gen_cluster,
                            start => {gen_cluster, start_link, [Config]}}]
                 end,

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
