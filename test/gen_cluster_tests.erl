-module(gen_cluster_tests).

-include_lib("eunit/include/eunit.hrl").

refresh_survives_unrelated_messages_test() ->
    with_cluster(30, #{}, fun(Pid) ->
        await_refresh(Pid),
        Pid ! harmless_info,
        gen_statem:cast(Pid, harmless_cast),
        await_refresh(Pid),
        Pid ! another_info,
        await_refresh(Pid)
    end).

infinity_disables_periodic_refresh_test() ->
    with_cluster(infinity, #{}, fun(Pid) ->
        await_refresh(Pid),
        Pid ! harmless_info,
        gen_statem:cast(Pid, harmless_cast),
        {active, _} = sys:get_state(Pid),
        receive {refresh, Pid} -> error(unexpected_refresh)
        after 100 -> ok
        end
    end).

code_change_preserves_state_and_refresh_test() ->
    with_cluster(30, #{}, fun(Pid) ->
        await_refresh(Pid),
        Before = sys:get_state(Pid),
        ok = sys:suspend(Pid),
        try
            ?assertEqual(ok, sys:change_code(Pid, gen_cluster, old_version, []))
        after
            ok = sys:resume(Pid)
        end,
        ?assertEqual(Before, sys:get_state(Pid)),
        await_refresh(Pid)
    end).

peer_identity_ignores_metadata_test_() ->
    Node = 'connected@gc.test',
    [?_test(assert_only_new_peer_connected(Member, Discovered))
     || {Member, Discovered} <-
            [{#{node => Node}, #{node => Node, port => 12345}},
             {#{node => Node, port => 12345}, #{node => Node}},
             {#{node => Node, port => 12345}, #{node => Node, port => 54321}},
             {#{node => Node}, #{node => Node}}]].

assert_only_new_peer_connected(Member, Discovered) ->
    NewPeer = #{node => 'new@gc.test', port => 23456},
    with_cluster(infinity, #{members => [Member], peers => [Discovered, NewPeer]}, fun(Pid) ->
        await_refresh(Pid),
        {active, _} = sys:get_state(Pid),
        ?assertEqual(NewPeer, await_connect(Pid)),
        receive {connect, Pid, Extra} -> error({unexpected_connect, Extra})
        after 0 -> ok
        end
    end).

failed_connections_are_retried_test() ->
    Peer = #{node => 'unreachable@gc.test', port => 12345},
    with_cluster(30, #{peers => [Peer], connect_result => false}, fun(Pid) ->
        await_refresh(Pid),
        ?assertEqual(Peer, await_connect(Pid)),
        await_refresh(Pid),
        ?assertEqual(Peer, await_connect(Pid))
    end).

await_connect(Pid) ->
    receive {connect, Pid, Peer} -> Peer
    after 1000 -> error(connect_missing)
    end.

with_cluster(Interval, Options, Fun) ->
    Config = Options#{owner => self()},
    {ok, Pid} = gen_cluster:start_link([{discovery, {gc_test_callback, Config}},
                                       {dist, {gc_test_callback, Config}},
                                       {refresh_interval_ms, Interval}]),
    try Fun(Pid)
    after gen_statem:stop(Pid)
    end.

await_refresh(Pid) ->
    receive {refresh, Pid} -> ok
    after 1000 -> error(refresh_missing)
    end.
