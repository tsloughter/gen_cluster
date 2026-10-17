-module(gc_test_dns).

-export([with_records/2, srv/2, ptr/1]).

%% Exercise the real resolver against a loopback DNS server. Tests using this
%% helper must run sequentially because resolver configuration is VM-wide.
with_records(Records, Fun) ->
    Keys = [resolv_conf, nameservers, alt_nameservers, search, domain, retry],
    Saved = [{Key, inet_db:res_option(Key)} || Key <- Keys],
    {ok, Socket} = gen_udp:open(0, [binary, {ip, {127, 0, 0, 1}}, {active, true}]),
    {ok, Port} = inet:port(Socket),
    Server = spawn_link(fun() -> serve(Socket, Records) end),
    ok = gen_udp:controlling_process(Socket, Server),
    try
        ok = inet_db:res_option(resolv_conf, ""),
        ok = inet_db:res_option(nameservers, [{{127, 0, 0, 1}, Port}]),
        ok = inet_db:res_option(alt_nameservers, []),
        ok = inet_db:res_option(search, []),
        ok = inet_db:res_option(domain, ""),
        ok = inet_db:res_option(retry, 1),
        ok = inet_db:clear_cache(),
        Fun()
    after
        lists:foreach(fun({Key, Value}) -> inet_db:res_option(Key, Value) end, Saved),
        inet_db:clear_cache(),
        Ref = monitor(process, Server),
        Server ! stop,
        receive {'DOWN', Ref, process, Server, normal} -> ok
        after 1000 -> error(dns_server_did_not_stop)
        end
    end.

srv(Port, Host) ->
    <<0:16, 0:16, Port:16, (name(Host))/binary>>.

ptr(Host) ->
    name(Host).

serve(Socket, Records) ->
    receive
        {udp, Socket, IP, Port, Packet} ->
            case response(Packet, Records) of
                drop -> ok;
                Response -> ok = gen_udp:send(Socket, IP, Port, Response)
            end,
            serve(Socket, Records);
        stop ->
            gen_udp:close(Socket)
    end.

response(<<Id:16, _:16, 1:16, _:48, Question/binary>>, Records) ->
    {Labels, <<Type:16, Class:16, _/binary>>} = labels(Question, []),
    Domain = string:join(Labels, "."),
    Query = <<(name(Domain))/binary, Type:16, Class:16>>,
    case maps:get({Domain, Type}, Records, []) of
        drop ->
            drop;
        {error, RCode} ->
            <<Id:16, (16#8580 bor RCode):16, 1:16, 0:16, 0:16, 0:16, Query/binary>>;
        Answers ->
            Encoded = iolist_to_binary([answer(Type, Data) || Data <- Answers]),
            <<Id:16, 16#8580:16, 1:16, (length(Answers)):16, 0:16, 0:16,
              Query/binary, Encoded/binary>>
    end.

answer(Type, Data) ->
    <<16#C00C:16, Type:16, 1:16, 0:32, (byte_size(Data)):16, Data/binary>>.

name(Domain) ->
    iolist_to_binary([[<<(length(Label))>>, Label] || Label <- string:lexemes(Domain, ".")] ++ [<<0>>]).

labels(<<0, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
labels(<<Size, Label:Size/binary, Rest/binary>>, Acc) ->
    labels(Rest, [binary_to_list(Label) | Acc]).
