-module(gc_discover_epmd_all_tests).

-include_lib("eunit/include/eunit.hrl").

empty_hosts_return_no_peers_test() ->
    {ok, State} = gc_discover_epmd_all:init(#{hosts => []}),
    ?assertEqual(0, sets:size(gc_discover_epmd_all:peers(State))).
