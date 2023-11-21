-module(conman).

-export([is_connected/0
        ,is_disconnected/0
        ,check_connectivity/0
        ,get_disconnections/0]).

-spec is_connected() -> boolean().
is_connected() -> conman_watchtower:is_connected().

-spec is_disconnected() -> boolean().
is_disconnected() -> conman_watchtower:is_disconnected().

-spec check_connectivity() -> conman_watchtower:connection_status().
check_connectivity() -> conman_guard:check_connectivity().

-spec get_disconnections() -> non_neg_integer().
get_disconnections() -> conman_watchtower:get_disconnections().
