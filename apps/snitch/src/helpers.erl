-module(helpers).
-export([slab/1,format_string/1,format_list/2,is_subset/2]).


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

is_subset(ListA, ListB) ->
    SetA = sets:from_list(ListA),
    SetB = sets:from_list(ListB),
    sets:is_subset(SetA, SetB).