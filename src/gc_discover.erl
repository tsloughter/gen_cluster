-module(gc_discover).
-moduledoc """
Behaviour to implement to build a method for discovery nodes to connect to.
""".

-doc "The state of the callback.".
-type cb_state() :: term().

-export_type([cb_state/0]).

-doc "Called only when a `gen_cluster` first starts or restarts.".
-callback init(term()) -> {ok, cb_state()}.

-doc "Returns the peer nodes for `gen_cluster` to call `connect` on.".
-callback peers(cb_state()) -> gen_cluster:peers().
