-module(gc_discover_dns_ip_tests).

-include_lib("eunit/include/eunit.hrl").

reverse_lookup_timeout_test() ->
    Records = #{{"cluster.gc.test", 1} => [<<10, 0, 0, 1>>, <<10, 0, 0, 2>>],
                {"1.0.0.10.in-addr.arpa", 12} => drop,
                {"2.0.0.10.in-addr.arpa", 12} => [gc_test_dns:ptr("healthy.gc.test")]},
    gc_test_dns:with_records(Records, fun() ->
        {ok, State} = gc_discover_dns_ip:init(#{domain => "cluster.gc.test",
                                              host_type => hostname, lookup_timeout => 100}),
        {Elapsed, Peers} = timer:tc(gc_discover_dns_ip, peers, [State]),
        ?assert(Elapsed < 1000000),
        [NodeName, _] = string:split(atom_to_list(node()), "@"),
        ?assertEqual([#{node => list_to_atom(NodeName ++ "@healthy.gc.test")}], sets:to_list(Peers))
    end).
