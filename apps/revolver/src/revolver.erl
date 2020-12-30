-module(revolver).
-include_lib("kernel/src/inet_dns.hrl").

-export([lookup/2,lookup/3,lookup/4]).

lookup(Domain, Type) ->
    lookup(Domain, Type, dns_server()).
lookup(Domain, Type, NSs) ->
    Timeout = dns_timeout() * 1000,
    lookup(Domain, Type, NSs, Timeout).
lookup(Domain, Type, NSs, Timeout) ->
    {Status, Record} = inet_res:nnslookup(Domain, in, Type, NSs, Timeout),
    Reply = answers(Record),
    {Status, Reply}.

%% Internal Functions

%% TODO: Assumes port 53
%% TODO: random seed?
dns_server() ->
    Nss = dns_servers(),
    [{lists:nth(
        rand:uniform(erlang:length(Nss)),
        Nss),
      53}].

dns_servers() ->
    case application:get_env(?MODULE, dns_servers) of
        {ok, Value} -> Value;
        _ -> [{8,8,8,8},{8,8,4,4},{1,1,1,1},{9,9,9,9}]
    end.

dns_timeout() ->
    case application:get_env(?MODULE, dns_timeout) of
        {ok, Value} -> Value;
        _ -> 10
    end.

answers(#dns_rec{anlist=Answers})  -> parse(Answers);
answers(Error) when is_atom(Error) -> Error.

parse([])                      -> [];
parse([#dns_rr{}=X|Xs])        -> [parse(X)] ++ parse(Xs);
parse(#dns_rr{type = a}=X)     -> inet:ntoa(X#dns_rr.data);
parse(#dns_rr{type = aaaa}=X)  -> inet:ntoa(X#dns_rr.data);
parse(#dns_rr{type = mx}=X)    -> erlang:element(2, X#dns_rr.data);
parse(#dns_rr{type = soa}=X)   -> erlang:integer_to_list(
                                    erlang:element(3, X#dns_rr.data));
parse(#dns_rr{type = ns}=X)    -> X#dns_rr.data;
parse(#dns_rr{type = cname}=X) -> X#dns_rr.data;
parse(#dns_rr{type = txt}=X)   -> lists:nth(1,X#dns_rr.data).
