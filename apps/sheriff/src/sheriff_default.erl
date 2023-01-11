-module(sheriff_default).
-export([dns_timeout/0]).
-define(DEFAULT_TIMEOUT_SECONDS, 5).

dns_timeout() ->
    case application:get_env(?MODULE, dns_timeout) of
        {ok, Value} -> Value;
        _ -> ?DEFAULT_TIMEOUT_SECONDS
    end.
