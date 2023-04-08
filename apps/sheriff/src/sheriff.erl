-module(sheriff).
-export([lookup/1]).

-spec lookup(string()) -> ok.
lookup(Domain) -> sheriff_holster:lookup(Domain).
