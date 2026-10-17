-module(gc_test_callback).

-export([init/1, peers/1, members/1, connect/2, disconnect/2]).

init(Config) ->
    {ok, Config}.

peers(#{owner := Owner}=Config) ->
    Owner ! {refresh, self()},
    sets:from_list(maps:get(peers, Config, []), [{version, 2}]).

members(Config) ->
    sets:from_list(maps:get(members, Config, []), [{version, 2}]).

connect(Peer, #{owner := Owner}) ->
    Owner ! {connect, self(), Peer},
    true.

disconnect(_, _) ->
    true.
