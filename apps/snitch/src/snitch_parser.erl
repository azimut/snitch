-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([query_all/1,
         query_and_store/2]).

%% API

query_all(Domain) ->
    {S, R} = snitch_resolver:do_query(Domain, cname),
    query_all(Domain, S, R).

%% Internal functions

%% TODO: soa
format_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
format_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
format_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
format_data(#dns_rr{type = txt}=X)   -> lists:nth(1,X#dns_rr.data).

answer_data(ok,#dns_rec{}=Record) -> answer_data(ok, Record#dns_rec.anlist);
answer_data(error,Error)          -> [Error];
answer_data(ok,[])                -> [];
answer_data(ok,[#dns_rr{}=X|Xs])  ->
    Data = [format_data(X)] ++ answer_data(ok,Xs),
    lists:sort(Data).

query_and_store(Domain, cname) -> % get full chain up to A
    {Status, Record} = snitch_resolver:do_query(Domain, a),
    Data = answer_data(Status,Record),
    snitch_store:store_and_alert(Domain, cname, Data);
query_and_store(Domain, Type)  -> % get only record requested
    {Status, Record} = snitch_resolver:do_pure(Domain, Type),
    Data = answer_data(Status, Record),
    snitch_store:store_and_alert(Domain, Type, Data).

query_all(Domain, ok, #dns_rec{anlist=[]}) ->
    query_and_store(Domain, mx),
    query_and_store(Domain, txt),
    query_and_store(Domain, aaaa),
    query_and_store(Domain, a),
    query_and_store(Domain, ns);
query_all(Domain, ok, #dns_rec{})  ->
    query_and_store(Domain, cname);
query_all(Domain, error, Error) ->
    snitch_store:store_and_alert(Domain, cname, Error).

