-module(sheriff_pony).
-export([express/4]).

express(From, Domain, NS, Timeout) ->
    {State, Data} = sheriff_revolver:lookup(Domain, cname, NS, Timeout),
    gishgallop(State, From, Domain, Data).

%% Internal Functions

gishgallop(ok,From,Domain,{nil,[]}) ->
    From ! lookup(Domain,a),
    From ! lookup(Domain,aaaa),
    From ! lookup(Domain,mx),
    From ! lookup(Domain,soa),
    From ! lookup(Domain,ns),
    From ! lookup(Domain,txt);
gishgallop(Status,From,Domain,Data) ->
    From ! {Status, Domain, cname, Data}.

lookup(Domain, Type) ->
    {Status, Data} = sheriff_revolver:lookup(Domain, Type),
    {Status, Domain, Type, Data}.
