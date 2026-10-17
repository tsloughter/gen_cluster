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
