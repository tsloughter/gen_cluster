-module(gc_dist).

-type cb_state() :: term().

-callback init(cb_state()) -> {ok, cb_state()}.
-callback members(cb_state()) -> gen_cluster:peers().
-callback connect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.
-callback disconnect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.
