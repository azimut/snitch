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
    alert_on_difference(Domain, Type, Dns, Ets),
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

notify(Title, Msg) ->
    S = io_lib:format("/usr/bin/notify-send --urgency critical '~s' '~s'", [Title, Msg]),
    os:cmd(S).

alert_change(Domain, Type, Old, New) ->
    io:format("CHANGE! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type (~s) changed", [Type]),
    helpers:format_string(Msg),
    notify(Domain, Msg),
    helpers:format_list("OLD ~s~n", Old),
    helpers:format_list("NEW ~s~n", New).

alert_new(Domain, Type, H) ->
    io:format("NEW! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type ~s has new data:~n~s",[Type, H]),
    helpers:format_string(Msg),
    notify(Domain, Msg).

alert(_,_,_,_,hot)                 -> ok;
alert(Domain, Type, New=[H|_],_,cold)
  when length(New) =:= 1 ->
    alert_new(Domain, Type, H);
alert(Domain, Type, Dns, Ets,cold) -> alert_change(Domain, Type, Dns, Ets).

alert(Domain, Type, Dns, Ets) ->
    DateTime = lookup_datetime(Domain, Type),
    Life = snitch_tempo:datetime_older_than_seconds(DateTime,2*24*60*60),
    alert(Domain,Type,Dns,Ets,Life).

alert_on_difference(_,_,_Dns=[_|_],_Ets=[]) -> ok; % First time
alert_on_difference(_,_,Idem,Idem)          -> ok;
alert_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> alert(Domain, Type, Dns, Ets)
    end.

store_on_difference(_,_,_Dns=[],_)          -> ok;
store_on_difference(_,_,Idem,Idem)          -> ok;
store_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> store(Domain, Type, Dns)
    end.
