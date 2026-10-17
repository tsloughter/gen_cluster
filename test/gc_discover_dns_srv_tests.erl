-module(gc_discover_dns_srv_tests).

-include_lib("eunit/include/eunit.hrl").

resolved_ip_test_() ->
    [?_test(assert_resolved_ip(false, 1, <<10, 0, 0, 1>>, "10.0.0.1")),
     ?_test(assert_resolved_ip(true, 28, <<16#2001:16, 16#db8:16, 0:80, 1:16>>, "2001:db8::1"))].

assert_resolved_ip(IPv6, Type, Address, ExpectedHost) ->
    Records = #{{"_cluster._tcp.gc.test", 33} => [gc_test_dns:srv(12345, "target.gc.test")],
                {"target.gc.test", Type} => [Address]},
    gc_test_dns:with_records(Records, fun() ->
        {ok, State} = gc_discover_dns_srv:init(#{domain => "_cluster._tcp.gc.test",
                                               host_type => ip,
                                               ipv6 => IPv6}),
        [NodeName, _] = string:split(atom_to_list(node()), "@"),
        Expected = #{node => list_to_atom(NodeName ++ "@" ++ ExpectedHost), port => 12345},
        ?assertEqual([Expected], sets:to_list(gc_discover_dns_srv:peers(State)))
    end).

multiple_addresses_test_() ->
    [?_test(assert_addresses(false, 1, [<<10, 0, 0, 1>>, <<10, 0, 0, 2>>],
                             ["10.0.0.1", "10.0.0.2"])),
     ?_test(assert_addresses(true, 28,
                             [<<16#2001:16, 16#db8:16, 0:80, 1:16>>,
                              <<16#2001:16, 16#db8:16, 0:80, 2:16>>],
                             ["2001:db8::1", "2001:db8::2"]))].

assert_addresses(IPv6, Type, Addresses, ExpectedHosts) ->
    Records = #{{"_cluster._tcp.gc.test", 33} => [gc_test_dns:srv(12345, "target.gc.test")],
                {"target.gc.test", Type} => Addresses},
    gc_test_dns:with_records(Records, fun() ->
        {ok, State} = gc_discover_dns_srv:init(#{domain => "_cluster._tcp.gc.test",
                                               host_type => ip, ipv6 => IPv6}),
        ?assertEqual(expected_peers(ExpectedHosts), gc_discover_dns_srv:peers(State))
    end).

failed_target_does_not_hide_healthy_target_test_() ->
    [?_test(assert_failed_target({error, 3})),  % NXDOMAIN
     ?_test(assert_failed_target({error, 2})),  % SERVFAIL
     ?_test(assert_failed_target([]))].        % no address records

assert_failed_target(Failure) ->
    Records = #{{"_cluster._tcp.gc.test", 33} =>
                    [gc_test_dns:srv(12345, "missing.gc.test"),
                     gc_test_dns:srv(12345, "healthy.gc.test")],
                {"missing.gc.test", 1} => Failure,
                {"healthy.gc.test", 1} => [<<10, 0, 0, 1>>]},
    gc_test_dns:with_records(Records, fun() ->
        {ok, State} = gc_discover_dns_srv:init(#{domain => "_cluster._tcp.gc.test", host_type => ip}),
        ?assertEqual(expected_peers(["10.0.0.1"]), gc_discover_dns_srv:peers(State))
    end).

hostname_target_test() ->
    Records = #{{"_cluster._tcp.gc.test", 33} => [gc_test_dns:srv(12345, "target.gc.test")]},
    gc_test_dns:with_records(Records, fun() ->
        {ok, State} = gc_discover_dns_srv:init(#{domain => "_cluster._tcp.gc.test"}),
        ?assertEqual(expected_peers(["target.gc.test"]), gc_discover_dns_srv:peers(State))
    end).

expected_peers(Hosts) ->
    [NodeName, _] = string:split(atom_to_list(node()), "@"),
    sets:from_list([#{node => list_to_atom(NodeName ++ "@" ++ Host), port => 12345}
                    || Host <- Hosts], [{version, 2}]).
