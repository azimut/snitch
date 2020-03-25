-module(snitch_store).

-define(TABLE_NAME, mytable).
-export(
   [
    store/3,
    store_and_alert/3,
    lookup/2
   ]).

%% Flatten nested list of strings
%% https://stackoverflow.com/questions/2911420/erlang-flattening-a-list-of-strings
slab([]) ->
    [];
slab([F|R]) ->
    case io_lib:char_list(F) of
        true -> [F|slab(R)];
        false -> slab(F) ++ slab(R)
    end.

lookup(Domain, Type) ->
    slab(
      ets:match(?TABLE_NAME, {Domain, Type, '$1'})).

store(Domain, Type, List) ->
    ets:insert(?TABLE_NAME, {Domain, Type, List}),
    List.

alert(Domain, Type, New, Old) ->
    io:format("ALERT! domain (~s) changed for type (~s)~n",
              [Domain,Type]),
    lists:foreach(fun (S) -> io:format("OLD~n~s~n",[S]) end, Old),
    lists:foreach(fun (S) -> io:format("NEW~n~s~n",[S]) end, New),
    true.

alert_on_difference(_Domain, _Type, ListA,ListA) -> false;
alert_on_difference(_Domain, _Type, _New, [])    -> false;
alert_on_difference(Domain, Type, New, Old)      ->
    alert(Domain, Type, New, Old).

store_and_alert(Domain, Type, List) ->
    Lookup = lookup(Domain, Type),
    alert_on_difference(Domain, Type, List, Lookup),
    store(Domain, Type, List).
