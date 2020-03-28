-module(snitch_store).

-include("db.hrl").
-export(
   [
    store/3,
    store_and_alert/3
   ]).

%% API

store(Domain, Type, Data) ->
    Time = calendar:local_time(),
    ets:insert(?TABLE_NAME, {Domain, Type, Data, Time}).

store_and_alert(Domain, Type, ListNew) ->
    ListOld = lookup(Domain, Type),
    alert_on_difference(Domain, Type, ListNew, ListOld),
    store_on_difference(Domain, Type, ListNew, ListOld).

%% Private Functions

lookup(Domain, Type) ->
    lists:sort(
      helpers:slab(
        ets:match(?TABLE_NAME, {Domain, Type, '$1', '_'}))).

notify(Title, Msg) ->
    S = io_lib:format("/usr/bin/notify-send --urgency critical '~s' '~s'", [Title, Msg]),
    os:cmd(S).

alert(Domain, Type, New=[H|_],_)
  when erlang:length(New) =:= 1 ->
    io:format("NEW! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type ~s has new data:~n~s",[Type, H]),
    helpers:format_string(Msg),
    notify(Domain, Msg);
alert(Domain, Type, New, Old) ->
    io:format("CHANGE! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type (~s) changed", [Type]),
    helpers:format_string(Msg),
    notify(Domain, Msg),
    helpers:format_list("OLD ~s~n", Old),
    helpers:format_list("NEW ~s~n", New).

alert_on_difference(_,_,_Dns=[_|_],_Ets=[]) -> ok; % First time
alert_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> alert(Domain, Type, Dns, Ets)
    end.

store_on_difference(_,_,_Dns=[],_)          -> ok;
store_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> store(Domain, Type, Dns)
    end.
