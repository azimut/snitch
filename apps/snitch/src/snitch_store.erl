-module(snitch_store).

-define(TABLE_NAME, mytable).
-export(
   [
    store/3,
    store_and_alert/3
   ]).


%% API

store(Domain, Type, List) ->
    ets:insert(?TABLE_NAME, {Domain, Type, List}).

store_and_alert(Domain, Type, ListNew) ->
    ListOld = lookup(Domain, Type),
    alert_on_difference(Domain, Type, ListNew, ListOld),
    store(Domain, Type, ListNew).

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

format_list(Format, List) -> 
    lists:foreach(fun (S) -> io:format(Format, [S]) end, List).

lookup(Domain, Type) ->
    lists:sort(
      slab(
        ets:match(?TABLE_NAME, {Domain, Type, '$1'}))).

alert(Domain, Type, New, _)
  when erlang:length(New) =:= 1 ->
    io:format("ALERT! domain (~s) has new thing for type (~s)~n", [Domain,Type]),
    format_list("NEW ~s~n", New);
alert(Domain, Type, New, Old) ->
    io:format("ALERT! domain (~s) changed for type (~s)~n", [Domain,Type]),
    format_list("OLD ~s~n", Old),
    format_list("NEW ~s~n", New).

alert_on_difference(_, _, _, [])                    -> false;
alert_on_difference(_, _, [], _)                    -> false;
alert_on_difference(Domain, Type, ListNew, ListOld) ->
    SetNew = sets:from_list(ListNew),
    SetOld = sets:from_list(ListOld),
    case sets:is_subset(SetNew, SetOld) of
        true  -> ok;
        false -> alert(Domain, Type, ListNew, ListOld)
    end.

