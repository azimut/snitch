-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([process_domain/1]).

answer_data([]) ->
    [];
answer_data([#dns_rr{}=X|Xs]) ->
    Data = [format_data(X)] ++ answer_data(Xs),
    lists:sort(Data).

format_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
format_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
format_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
format_data(#dns_rr{type = txt}=X)   -> lists:nth(0, X#dns_rr.data).

store_record(_Domain, _Type, _LRecord) ->
    true or false.

query_and_store(Domain, cname) ->
    IsCName = fun (R) -> R#dns_rr.type == cname end,
    A = snitch_resolver:do_query(Domain, a),
    CName = lists:filter(IsCName, A),
    Data = answer_data(CName),
    store_record(Domain, cname, Data);
query_and_store(Domain, Type) ->
    Records = snitch_resolver:do_query(Domain, Type),
    Data = answer_data(Records),
    store_record(Domain, Type, Data).

process_domain(Domain) ->
    CName = snitch_resolver:do_query(Domain, cname),
    proces_domain(Domain, CName).
process_domain(Domain, [])  ->
    snitch_resolver:do_query(Domain, mx);
process_domain(Domain, [_]) ->

