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
