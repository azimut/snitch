-module(pony).
-export([gishgallop/4,express/2,express/3]).

express(From, Domain)       ->
    {State, Domain, cname, Data} = revolver:lookup(Domain, cname),
    gishgallop(State, From, Domain, Data).
express(From, Domain, Type) ->
    From ! revolver:lookup(Domain, Type).

gishgallop(error,From,Domain,Error) ->
    From ! {error,From,Domain,cname,Error};
gishgallop(ok,From,Domain,[])       ->
    From ! revolver:lookup(Domain, a),
    From ! revolver:lookup(Domain, aaaa),
    From ! revolver:lookup(Domain, mx),
    From ! revolver:lookup(Domain, soa),
    From ! revolver:lookup(Domain, ns),
    From ! revolver:lookup(Domain, txt);
gishgallop(ok,From,Domain,Data)     ->
    From ! {ok,From,Domain,cname,Data}.
