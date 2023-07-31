-module(clock).
-export([state/0]).

-spec state() -> [{string(), non_neg_integer()}].
state() -> clock_clock:get_state().
