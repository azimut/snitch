-module(snitch_store).

-include("db.hrl").
-export(
   [
    init/0,
    delete/1,
    store/3,
    store_and_alert/3,
    lookup/2,
    lookup_datetime/2
   ]).

%% API

init() ->
    ets:new(?TABLE_NAME, [bag, public, named_table]).

delete(Domain) ->
    ets:delete(?TABLE_NAME, Domain).

store(Domain, Type, Data) ->
    Time = calendar:local_time(),
    ets:insert(?TABLE_NAME, {Domain, Type, Data, Time}).

store_and_alert(Domain, Type, Dns) ->
    Ets = lookup(Domain, Type),
    snitch_alert:alert_on_difference(Domain, Type, Dns, Ets),
    store_on_difference(Domain, Type, Dns, Ets).

%% Private Functions

lookup(Domain, Type) ->
    lists:sort(
      helpers:slab(
        ets:match(?TABLE_NAME, {Domain, Type, '$1', '_'}))).

lookup_datetime(Domain, Type) ->
    erlang:hd(
      lists:sort(
        lists:flatten(
          ets:match(?TABLE_NAME, {Domain, Type,'_','$1'})))).

store_on_difference(_,_,_Dns=[],_)          -> ok;
store_on_difference(_,_,Idem,Idem)          -> ok;
store_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> store(Domain, Type, Dns)
    end.
