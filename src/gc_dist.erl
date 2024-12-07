-module(gc_dist).
-moduledoc "
Behaviour to implement to build a new method for connecting nodes into a
cluster.
".

-doc "The state of the callback.".
-type cb_state() :: term().

-export_type([cb_state/0]).

-doc "Called only when a `gen_cluster` first starts or restarts.".
-callback init(cb_state()) -> {ok, cb_state()}.

-doc "Return the current members connected in the cluster.".
-callback members(cb_state()) -> gen_cluster:peers().

-doc "Connect the current node to the argument peer.".
-callback connect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.

-doc "Disconnect the peer from the current node.".
-callback disconnect(gen_cluster:peer(), cb_state()) -> boolean() | ignored.
