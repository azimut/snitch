-module(snitch_locus).
-include("db.hrl").
-export([asn/1, asns/1]).
-export([new_asns/2]).

asn(Ip) ->
    {ok, Record} = locus:lookup(asn, Ip),
    maps:get(<<"autonomous_system_organization">>,
             Record).

asns(Ips) ->
    [ asn(X) || X <- Ips ].

new_asns(DnsIps, EtsIps) ->
    DnsAsns = sets:from_lists(asns(DnsIps)),
    EtsAsns = sets:from_lists(asns(EtsIps)),
    sets:to_list(
      sets:subtract(DnsAsns,EtsAsns)).
