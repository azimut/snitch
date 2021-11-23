-module(sheriff_default).

-define(DEFAULT_TIMEOUT_SECONDS,5).
-define(DEFAULT_DNS_SERVERS,[{8,8,8,8},{8,8,4,4},{1,1,1,1},{9,9,9,9}]).

-export([dns_servers/0,dns_timeout/0]).

dns_servers() ->
    case application:get_env(?MODULE, dns_servers) of
        {ok, Value} -> Value;
        _ -> ?DEFAULT_DNS_SERVERS
    end.

dns_timeout() ->
    case application:get_env(?MODULE, dns_timeout) of
        {ok, Value} -> Value;
        _ -> ?DEFAULT_TIMEOUT_SECONDS
    end.
