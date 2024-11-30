-module(gc_discover).

-type cb_state() :: term().

-callback init(term()) -> {ok, cb_state()}.
-callback peers(cb_state()) -> gen_cluster:peers().
