-module(sheriff_pony).
-export([express/4]).

express(From, Domain, NS, Timeout) ->
    {State, Data} = sheriff_revolver:lookup(Domain, cname, NS, Timeout),
    gishgallop(State,NS,Timeout,From,Domain,Data).

%% Internal Functions

gishgallop(ok,NS,Timeout,From,Domain,{nil,[]}) ->
    From ! lookup(Domain,NS,Timeout,a),
    From ! lookup(Domain,NS,Timeout,aaaa),
    From ! lookup(Domain,NS,Timeout,mx),
    From ! lookup(Domain,NS,Timeout,soa),
    From ! lookup(Domain,NS,Timeout,ns),
    From ! lookup(Domain,NS,Timeout,txt);
gishgallop(Status,NS,_Timeout,From,Domain,Data) ->
    From ! {Status, ntoa(NS), Domain, cname, Data}.

lookup(Domain, NS, Timeout, Type) ->
    {Status, Data} = sheriff_revolver:lookup(Domain, Type, NS, Timeout),
    {Status, ntoa(NS), Domain, Type, Data}.

ntoa([{Address,_Port}]) -> inet:ntoa(Address).
