-module(sheriff_revolver).
-include_lib("kernel/src/inet_dns.hrl").
-export([lookup/4]).

lookup(Domain, Type, NS, Timeout) ->
    {Status, Record} = inet_res:nnslookup(Domain, in, Type, NS, Timeout),
    Reply = answers(Record),
    {Status, Reply}.

%% Internal Functions

answers(#dns_rec{anlist=Answers}) ->
    { answers_type(Answers) , parse(Answers) };
answers(Error)
  when is_atom(Error) ->
    Error.

%% NOTE: assumes all types returns are the same
answers_type([])                     -> nil;
answers_type([#dns_rr{type=Type}|_]) -> Type.

parse([])                         -> [];
parse([X=#dns_rr{type = soa}|Xs]) ->  parse(X)  ++ parse(Xs);
parse([X|Xs])                     -> [parse(X)] ++ parse(Xs);
parse(X=#dns_rr{type = aaaa})     -> inet:ntoa(X#dns_rr.data);
parse(X=#dns_rr{type = a})        -> inet:ntoa(X#dns_rr.data);
parse(X=#dns_rr{type = cname})    -> X#dns_rr.data;
parse(X=#dns_rr{type = mx})       -> erlang:element(2, X#dns_rr.data);
parse(X=#dns_rr{type = ns})       -> X#dns_rr.data;
parse(X=#dns_rr{type = soa})      -> lists:filter(fun erlang:is_list/1,
                                                  erlang:tuple_to_list(X#dns_rr.data));
parse(X=#dns_rr{type = txt})      -> lists:nth(1,X#dns_rr.data).
