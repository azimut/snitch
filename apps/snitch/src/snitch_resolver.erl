-module(snitch_resolver).

-include_lib("kernel/src/inet_dns.hrl").
-include("db.hrl").

-export(
   [
    do_query/2,
    do_pure/2
   ]).

get_dns_server() ->
    [{lists:nth(
        rand:uniform(erlang:length(?DNS_SERVERS)),
        ?DNS_SERVERS),
      53}].

do_query(Domain, Type) ->
    NSs = get_dns_server(),
    TimeOut = ?DNS_TIMEOUT * 1000,
    {ok, R} = inet_res:nnslookup(Domain, in, Type, NSs, TimeOut),
    R#dns_rec.anlist.

do_pure(Domain, Type) ->
    Answers = do_query(Domain, Type),
    lists:filter(fun (R) -> R#dns_rr.type == Type end, Answers).
