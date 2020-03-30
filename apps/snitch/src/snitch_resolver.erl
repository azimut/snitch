-module(snitch_resolver).

-include_lib("kernel/src/inet_dns.hrl").
-include("db.hrl").

-export(
   [
    do_query/2,
    do_pure/2,
    get_dns_server/0
   ]).

%% Api

do_query(Domain, Type) ->
    NSs = get_dns_server(),
    inet_res:nnslookup(Domain, in, Type, NSs, ?DNS_TIMEOUT * 1000).

do_pure(Domain, Type) ->
    {Status, Record} = do_query(Domain, Type),
    purify(Status, Record, Type).

%% Internal functions

get_dns_server() ->
    [{lists:nth(
        rand:uniform(erlang:length(?DNS_SERVERS)),
        ?DNS_SERVERS),
      53}].

purify(error, Error, _Type)         ->
    {error, Error};
purify(ok, #dns_rec{}=Record, Type) ->
    Answers = Record#dns_rec.anlist,
    NewAnswers = lists:filter(fun (R) -> R#dns_rr.type == Type end, Answers),
    {ok, Record#dns_rec{anlist=NewAnswers}}.


