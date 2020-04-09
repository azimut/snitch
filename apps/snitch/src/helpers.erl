-module(helpers).
-export([slab/1,format_string/1,format_list/2,is_subset/2,subtract/2]).
-export([remove_duplicates/1]).
-export([is_ip/1,is_not_ip/1]).

%% Flatten nested list of strings
%% https://stackoverflow.com/questions/2911420/erlang-flattening-a-list-of-strings
slab([])                  -> [];
slab([F|R])               ->
    case io_lib:char_list(F) of
        true -> [F|slab(R)];
        false -> slab(F) ++ slab(R)
    end;
slab(F) when is_number(F) -> [F];
slab(F) when is_atom(F)   -> [F].

format_string(String) ->
    io:format("~s~n",[String]).

format_list(Format, List) -> 
    lists:foreach(fun (S) -> io:format(Format, [S]) end, List).

is_subset(ListA, ListB) ->
    SetA = sets:from_list(ListA),
    SetB = sets:from_list(ListB),
    sets:is_subset(SetA, SetB).

subtract(ListA, ListB) ->
    SetA = sets:from_list(ListA),
    SetB = sets:from_list(ListB),
    sets:to_list(
      sets:subtract(SetA, SetB)).

remove_duplicates(L) -> ordsets:to_list(ordsets:from_list(L)).

is_ip(S) ->
    case re:run(S, "[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") of
        {match, _} -> true;
        nomatch    -> false
    end.

is_not_ip(S) -> not is_ip(S).
