-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([query_all/1,
         query_and_store/2]).

%% API

query_all(Domain) ->
    {S, R} = snitch_resolver:do_query(Domain, cname),
    query_all(Domain, S, R).

%% Internal functions

get_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
get_data(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
get_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
get_data(#dns_rr{type = soa}=X)   -> erlang:integer_to_list(
                                       erlang:element(3, X#dns_rr.data));
get_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
get_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
get_data(#dns_rr{type = txt}=X)   -> lists:nth(1,X#dns_rr.data).

get_answers([])               -> [];
get_answers([#dns_rr{}=X|Xs]) -> [get_data(X)] ++ get_answers(Xs).

answers(ok, #dns_rec{}=Record) -> lists:sort(get_answers(Record#dns_rec.anlist));
answers(error, Error)          -> [Error].

query_and_store(Domain, cname) -> % get full chain up to A
    {Status, Record} = snitch_resolver:do_query(Domain, a),
    Dns = answers(Status,Record),
    snitch_store:store_and_alert(Domain, cname, Dns);
query_and_store(Domain, Type)  -> % get only record requested
    {Status, Record} = snitch_resolver:do_pure(Domain, Type),
    Dns = answers(Status, Record),
    snitch_store:store_and_alert(Domain, Type, Dns).

query_all(Domain, ok, #dns_rec{anlist=[]}) ->
    query_and_store(Domain, mx),
    query_and_store(Domain, txt),
    query_and_store(Domain, soa),
    query_and_store(Domain, aaaa),
    query_and_store(Domain, a),
    query_and_store(Domain, ns);
query_all(Domain, ok, #dns_rec{})          ->
    query_and_store(Domain, cname);
query_all(Domain, error, Error)            ->
    snitch_store:store_and_alert(Domain, cname, [Error]).

