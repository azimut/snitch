-module(sheriff_revolver).
-include_lib("kernel/src/inet_dns.hrl").
-include("header.hrl").
-export([shoot/4]).

-spec shoot(pid(), string(), inet_res:nameserver(), non_neg_integer())
           -> {ok, #results{}} | {error, #error{}}.
shoot(From, Domain, NS, Timeout) ->
    case lookup(Domain, cname, NS, Timeout) of
        {ok, #results{answers = []}} ->
            From ! lookup(Domain,a,NS,Timeout),
            From ! lookup(Domain,aaaa,NS,Timeout),
            From ! lookup(Domain,mx,NS,Timeout),
            From ! lookup(Domain,soa,NS,Timeout),
            From ! lookup(Domain,ns,NS,Timeout),
            From ! lookup(Domain,txt,NS,Timeout);
        {ok, #results{}=Results} ->
            From ! {ok, Results};
        {error, Error} ->
            From ! {error, Error}
    end.

-spec lookup(string(), inet_res:dns_rr_type(), inet_res:nameserver(), non_neg_integer())
            -> {ok, #results{}} | {error, #error{}}.
lookup(Domain, Type, NS, Timeout) ->
    [{NSAddress,_}] = NS,
    case inet_res:nnslookup(Domain, in, Type, NS, Timeout) of
        {ok, Record} ->
            {ok, #results{type    = Record#dns_rec.anlist#dns_rr.type,
                          server  = inet:ntoa(NSAddress),
                          answers = answers(Record#dns_rec.anlist)}};
        {error, Error} ->
            io:format("sheriff_revolver:lookup : Reason = ~p~n",[Error]),
            {error, #error{type   = Type,
                           server = inet:ntoa(NSAddress),
                           domain = Domain,
                           reason = Error}}
    end.

-spec answers(list(#dns_rr{})) -> list(string()).
answers([])                         -> [];
answers([X=#dns_rr{type = soa}|Xs]) ->  answers(X)  ++ answers(Xs);
answers([X|Xs])                     -> [answers(X)] ++ answers(Xs);
answers(X=#dns_rr{type = aaaa})     -> inet:ntoa(X#dns_rr.data);
answers(X=#dns_rr{type = a})        -> inet:ntoa(X#dns_rr.data);
answers(X=#dns_rr{type = cname})    -> X#dns_rr.data;
answers(X=#dns_rr{type = mx})       -> erlang:element(2, X#dns_rr.data);
answers(X=#dns_rr{type = ns})       -> X#dns_rr.data;
answers(X=#dns_rr{type = soa})      -> lists:filter(fun erlang:is_list/1,
                                                    erlang:tuple_to_list(X#dns_rr.data));
answers(X=#dns_rr{type = txt})      -> lists:nth(1,X#dns_rr.data).
