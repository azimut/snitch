-module(clock).
-export([add/1, del/1, state/0, now_gregorian/0]).

-spec add(Domain :: string()) -> ok.
add(Domain) -> clock_clock:add(Domain).

-spec del(Domain :: string()) -> ok.
del(Domain) -> clock_clock:add(Domain).

-spec now_gregorian() -> non_neg_integer().
now_gregorian() -> clock_clock:now_gregorian().

state() -> clock_clock:get_state().
