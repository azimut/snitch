-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([query_all/1,
         query_and_store/2]).

%% API

query_all(Domain) ->
    CName = snitch_resolver:do_query(Domain, cname),
    query_all(Domain, CName).

%% Internal functions

%% TODO: soa
format_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
format_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
format_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
format_data(#dns_rr{type = txt}=X)   -> lists:nth(1,X#dns_rr.data).

answer_data([])               -> [];
answer_data([#dns_rr{}=X|Xs]) ->
    Data = [format_data(X)] ++ answer_data(Xs),
    lists:sort(Data).

query_and_store(Domain, cname) ->
    IsCName = fun (R) -> R#dns_rr.type == cname end,
    A = snitch_resolver:do_query(Domain, a),
    CName = lists:filter(IsCName, A),
    Data = answer_data(CName),
    snitch_store:store_and_alert(Domain, cname, Data);
query_and_store(Domain, Type)  ->
    Records = snitch_resolver:do_pure(Domain, Type),
    Data = answer_data(Records),
    snitch_store:store_and_alert(Domain, Type, Data).

query_all(Domain, []) ->
    query_and_store(Domain, mx),
    query_and_store(Domain, txt),
    query_and_store(Domain, aaaa),
    query_and_store(Domain, a),
    query_and_store(Domain, ns);
query_all(Domain, _)  ->
    query_and_store(Domain, cname).
