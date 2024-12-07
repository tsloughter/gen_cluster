-module(gc_discover_dns).
-moduledoc "
Utility functions for working with DNS related types and options.
".

-export([ip_record_type/1]).

-doc "A doman name. A string with no adjacent dots. Defined until inet_res:dns_name() is exported.".
-type dns_name() :: string(). %% unexported: inet_res:dns_name()

-doc "Whether to query for `a` or `aaaa` records".
-type use_ipv6() :: boolean().

-doc "Whether the host part of a node name is a hostname, IPv4 or IPv6 address".
-type host_type() :: ip | hostname.

-doc "Type of IP record to query DNS for, `a` or `aaaa`".
-type ip_record_type() :: a | aaaa.

-export_type([dns_name/0,
              use_ipv6/0,
              host_type/0,
              ip_record_type/0]).

-doc "Returns `a` when `use_ipv6` is `false` and `aaaa` when it is `true`".
-spec ip_record_type(use_ipv6()) -> ip_record_type().
ip_record_type(true) ->
    aaaa;
ip_record_type(false) ->
    a.
