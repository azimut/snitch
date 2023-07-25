-module(clock).
-export([add/1, del/1, state/0]).

-spec add(Domain :: string()) -> ok.
add(Domain) -> clock_clock:add(Domain).

-spec del(Domain :: string()) -> ok.
del(Domain) -> clock_clock:add(Domain).

state() -> clock_clock:get_state().
