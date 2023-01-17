-module(sheriff).
-export([lookup/1]).

-spec lookup(string()) -> term().
lookup(Domain) ->
    sheriff_holster:lookup(Domain).
