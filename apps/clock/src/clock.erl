-module(clock).
-export([add/1, del/1]).

-spec add(string()) -> ok.
-spec del(string()) -> ok.
add(Domain) -> clock_clock:add(Domain).
del(Domain) -> clock_clock:add(Domain).
