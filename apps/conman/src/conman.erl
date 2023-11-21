-module(conman).

-export([is_connected/0
        ,is_disconnected/0]).

-spec is_connected() -> boolean().
is_connected() -> conman_watchtower:is_connected().

-spec is_disconnected() -> boolean().
is_disconnected() -> conman_watchtower:is_disconnected().
