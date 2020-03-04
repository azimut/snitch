-module(snitch_parser).
-include_lib("kernel/src/inet_dns.hrl").
-export([inspect_value/1]).

%% Description: Gets inmutable data from a [{dns_rr}]
%% lists of NS, MX, TXT
get_data([]) ->
    [];
get_data([#dns_rr{}=X|Xs]) ->
    Data = [format_data(X)] ++ get_data(Xs),
    lists:sort(Data).

format_data(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
format_data(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
format_data(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
format_data(#dns_rr{type = cname}=X) -> X#dns_rr.data;
format_data(#dns_rr{type = txt}=X)   -> lists:nth(0, X#dns_rr.data).

store_record(_Domain, _Type, _LRecord) ->
    true or false.

inspect_value(Domain) ->
    CName = snitch_resolver:do_query(Domain, cname),
    inspect_value(Domain, CName).
inspect_value(Domain, []) ->
    snitch_resolver:do_query(Domain, mx);
inspect_value(Domain, [_]) ->
    A = snitch_resolver:do_query(Domain, a),
    CName = lists:filter(fun (R) -> R#dns_rr.type == cname end, A),
    Data = get_data(CName),
    store_record(Domain, cname, Data).
