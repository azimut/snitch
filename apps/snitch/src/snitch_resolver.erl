-module(snitch_resolver).

-include_lib("kernel/src/inet_dns.hrl").
-define(DNS_SERVERS, [{8,8,8,8},{8,8,4,4},{1,1,1,1},{9,9,9,9}]).

-export([do_query/2]).

get_dns_server() ->
    [{lists:nth(rand:uniform(erlang:length(?DNS_SERVERS)),?DNS_SERVERS), 53}].

do_query(Domain, Type) ->
    NSs = get_dns_server(),
    TimeOut = 5 * 1000,
    {ok, R} = inet_res:nnslookup(Domain, in, Type, NSs, TimeOut),
    R#dns_rec.anlist.
