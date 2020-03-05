-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([process_domain/1]).

%% API

process_domain(Domain)    ->
    CName = snitch_resolver:do_query(Domain, cname),
    process_domain(Domain, CName).

%% Internal functions

process_domain(Domain, []) ->
    query_and_store(Domain, mx),
    query_and_store(Domain, txt),
    query_and_store(Domain, aaaa),
    query_and_store(Domain, a),
    query_and_store(Domain, ns);
process_domain(Domain, _)  ->
    query_and_store(Domain, cname).

format_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
format_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
format_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
format_data(#dns_rr{type = txt}=X)   -> lists:nth(1,X#dns_rr.data).

answer_data([]) ->
    [];
answer_data([#dns_rr{}=X|Xs]) ->
    Data = [format_data(X)] ++ answer_data(Xs),
    lists:sort(Data).

query_and_store(Domain, cname) ->
    IsCName = fun (R) -> R#dns_rr.type == cname end,
    A = snitch_resolver:do_query(Domain, a),
    CName = lists:filter(IsCName, A),
    Data = answer_data(CName),
    snitch_store:store(Domain, cname, Data);
query_and_store(Domain, Type)  ->
    Records = snitch_resolver:do_pure(Domain, Type),
    Data = answer_data(Records),
    snitch_store:store(Domain, Type, Data).
