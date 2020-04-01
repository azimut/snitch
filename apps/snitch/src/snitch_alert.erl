-module(snitch_alert).
-export([alert_on_difference/4]).

%% API

alert_on_difference(_,_,_Dns=[_|_],_Ets=[]) -> ok; % First time
alert_on_difference(_,_,Idem,Idem)          -> ok;
alert_on_difference(Domain, Type, Dns, Ets) ->
    case helpers:is_subset(Dns, Ets) of
        true  -> ok;
        false -> alert(Domain, Type, Dns, Ets)
    end.

%% Internal functions

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
alert(Domain, Type, Dns, Ets, cold) -> alert_change(Domain, Type, Dns, Ets).

alert(Domain, Type, Dns, Ets) ->
    DateTime = snitch_store:lookup_datetime(Domain, Type),
    Life = snitch_tempo:datetime_older_than_seconds(DateTime,2*24*60*60),
    alert(Domain,Type,Dns,Ets,Life).
