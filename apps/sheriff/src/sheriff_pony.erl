-module(sheriff_pony).
-export([express/2]).

express(From, Domain) ->
    {State, Data} = revolver:lookup(Domain, cname),
    gishgallop(State, From, Domain, Data).

%% Internal Functions

gishgallop(ok,From,Domain,[]) ->
    From ! lookup(Domain,a),
    From ! lookup(Domain,aaaa),
    From ! lookup(Domain,mx),
    From ! lookup(Domain,soa),
    From ! lookup(Domain,ns),
    From ! lookup(Domain,txt);
gishgallop(Status,From,Domain,Data) ->
    From ! lookup(Domain, cname, Status, Data).

lookup(Domain, Type) ->
    {Status, Data} = revolver:lookup(Domain, Type),
    lookup(Domain, Type, Status, Data).
lookup(Domain, Type, Status, Data) ->
    {Status, Data, Domain, Type}.
