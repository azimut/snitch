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

%% Flatten nested list of strings
%% https://stackoverflow.com/questions/2911420/erlang-flattening-a-list-of-strings
slab([])    ->
    [];
slab([F|R]) ->
    case io_lib:char_list(F) of
        true -> [F|slab(R)];
        false -> slab(F) ++ slab(R)
    end.

format_string(String) ->
    io:format("~s~n",[String]).

format_list(Format, List) -> 
    lists:foreach(fun (S) -> io:format(Format, [S]) end, List).

lookup(Domain, Type) ->
    lists:sort(
      slab(
        ets:match(?TABLE_NAME, {Domain, Type, '$1', '_'}))).

notify(Title, Msg) ->
    S = io_lib:format("/usr/bin/notify-send --urgency critical '~s' '~s'", [Title, Msg]),
    os:cmd(S).

alert(Domain, Type, New=[H|_],_)
  when erlang:length(New) =:= 1 ->
    io:format("NEW! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type ~s has new data:~n~s",[Type, H]),
    format_string(Msg),
    notify(Domain, Msg);
alert(Domain, Type, New, Old) ->
    io:format("CHANGE! data for domain ~s~n", [Domain]),
    Msg = io_lib:format("type (~s) changed", [Type]),
    format_string(Msg),
    notify(Domain, Msg),
    format_list("OLD ~s~n", Old),
    format_list("NEW ~s~n", New).

is_subset(ListA, ListB) ->
    SetA = sets:from_list(ListA),
    SetB = sets:from_list(ListB),
    sets:is_subset(SetA, SetB).

alert_on_difference(_,_,_Dns=[_|_],_Ets=[]) -> ok; % First time
alert_on_difference(Domain, Type, Dns, Ets) ->
    case is_subset(Dns, Ets) of
        true  -> ok;
        false -> alert(Domain, Type, Dns, Ets)
    end.

store_on_difference(_,_,_Dns=[],_)           -> ok;
store_on_difference(Domain, Type, Dns, Ets) ->
    case is_subset(Dns, Ets) of
        true  -> ok;
        false -> store(Domain, Type, Dns)
    end.
