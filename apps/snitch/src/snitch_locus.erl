-module(snitch_locus).
-include("db.hrl").
-export([asn/1, asns/1]).
-export([new_asns/2]).

new_asns(_,[]) -> [];
new_asns([],_) -> [];
new_asns(DnsIps, EtsIps) ->
    DnsAsns = sets:from_list(asns(DnsIps)),
    EtsAsns = sets:from_list(asns(EtsIps)),
    sets:to_list(
      sets:subtract(DnsAsns,EtsAsns)).

%% Private functions

asn(Ip) ->
    {Status, Record} = locus:lookup(asn, Ip),
    process(Status, Record, Ip).

asns(Ips) ->
    Query = [ asn(Ip) || Ip <- Ips ],
    [ Record || {Status, Record} <- Query, Status == ok].

process(ok,    Record, _Ip) ->
    {ok, maps:get(<<"autonomous_system_organization">>, Record)};
process(error, Record, Ip) ->
    logger:error("Cannot resolve ~s due ~s",[Ip, Record]),
    {error, Record}.
