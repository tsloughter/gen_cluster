-module(gc_discover_dns).

-export([ip_record_type/1]).

-type use_ipv6() :: boolean().
-type ip_record_type() :: a | aaaa.

-export_type([use_ipv6/0,
              ip_record_type/0]).

-spec ip_record_type(use_ipv6()) -> ip_record_type().
ip_record_type(true) ->
    aaaa;
ip_record_type(false) ->
    a.
