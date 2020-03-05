-module(snitch_store).

-include_lib("kernel/src/inet_dns.hrl").

-export([store/3]).

store(Domain, Type, LRecord) ->
    {Domain, Type, LRecord}.
