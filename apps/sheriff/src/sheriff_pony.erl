-module(sheriff_pony).
-export([express/2,express/3]).

express(From, Domain)       ->
    {State, Data} = revolver:lookup(Domain, cname),
    gishgallop(State, From, Domain, Data).
express(From, Domain, Type) ->
    From ! reply(Domain, Type).

%% Internal Functions

gishgallop(ok,From,Domain,[])       ->
    From ! reply(Domain,a),
    From ! reply(Domain,aaaa),
    From ! reply(Domain,mx),
    From ! reply(Domain,soa),
    From ! reply(Domain,ns),
    From ! reply(Domain,txt);
gishgallop(Status,From,Domain,Data) ->
    From ! {Status,Data,Domain,cname}.

reply(Domain, Type) ->
    {Status, Data} = revolver:lookup(Domain, cname),
    {Status, Data, Domain, Type}.
