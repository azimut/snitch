-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([get_unique/1]).

%% Description: Gets inmutable data from a [{dns_rr}]

get_unique([]) ->
    [];
get_unique([#dns_rr{}=X|Xs]) ->
    get_unique(X) ++ get_unique(Xs);
get_unique(#dns_rr{type = mx}=X) ->
    {_Priority, Data} = X#dns_rr.data,
    [Data];
get_unique(#dns_rr{type = ns}=X) ->
    [X#dns_rr.data];
get_unique(#dns_rr{type = txt}=X) ->
    X#dns_rr.data.

