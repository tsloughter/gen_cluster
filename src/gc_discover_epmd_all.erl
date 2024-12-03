-module(gc_discover_epmd_all).

-behaviour(gc_discover).

-export([init/1,
         peers/1]).

-type host() :: atom() | string() | inet:ip_address().

-record(state, {hosts :: [host()]}).

-spec init(#{hosts => [host()]}) -> {ok, #state{}}.
init(Opts) ->
    %% TODO: error if this fails
    [_, NodeHost] = string:split(atom_to_list(node()), "@"),
    Hosts = case maps:find(hosts, Opts) of
                error ->
                    [NodeHost];
                {ok, Hs=[_|_]} ->
                    %% replaces `localhost' with the hostname of the node
                    lists:map(fun(localhost) ->
                                      NodeHost;
                                 (Host) ->
                                      Host
                              end, Hs)
                %% return ignore on this failing? log an error?
            end,
    {ok, #state{hosts=Hosts}}.

-spec peers(#state{}) -> gen_cluster:peers().
peers(#state{hosts=Hosts}) ->
    Peers = sets:new([{version, 2}]),
    lists:foldl(fun(Host, PeersAcc) ->
                        case erl_epmd:names(Host) of
                            {ok, Names} ->
                                lists:foldl(fun({NodeName, NodePort}, PeersAcc1) ->
                                                    %% elp:ignore W0023
                                                    Node = list_to_atom(string:join([NodeName, to_string(Host)], "@")),
                                                    sets:add_element(#{node => Node,
                                                                       port => NodePort}, PeersAcc1)
                                            end, PeersAcc, Names);
                            {error, _} ->
                                %% TODO: log a info/warning/error?
                                PeersAcc
                        end
                end, Peers, Hosts).

%%

-spec to_string(atom() | string() | inet:ip_address()) -> string().
to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(IP) when is_tuple(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            "";
        S ->
            S
    end;
to_string(A) when is_list(A) ->
    A.
