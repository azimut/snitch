-module(sheriff_revolver).
-include_lib("kernel/src/inet_dns.hrl").
-include("header.hrl").
-export([shoot/4,lookup/4,answers/1]).

-spec shoot(pid(), string(), inet_res:nameserver(), non_neg_integer())
           -> {ok, #results{}} | {error, #error{}}.
shoot(From, Domain, NS, Timeout) ->
    case lookup(Domain, cname, NS, Timeout) of
        {error, #error{reason = record_not_found}} ->
            From ! lookup(Domain,a,NS,Timeout),
            From ! lookup(Domain,aaaa,NS,Timeout),
            From ! lookup(Domain,mx,NS,Timeout),
            From ! lookup(Domain,soa,NS,Timeout),
            From ! lookup(Domain,ns,NS,Timeout),
            From ! lookup(Domain,txt,NS,Timeout);
        Response -> From ! Response
    end.

-spec lookup(string(), inet_res:dns_rr_type(), inet_res:nameserver(), non_neg_integer())
            -> {ok, #results{}} | {error, #error{}}.
lookup(Domain, Type, {NSAddress,_}=NS, Timeout) ->
    case inet_res:nnslookup(Domain, in, Type, [NS], Timeout) of
        {error, Error} ->
            logger:notice("error : ~p for ~s/~p ~n",[Error, Domain, Type]),
            {error, #error{qtype  = Type,
                           server = NSAddress,
                           domain = Domain,
                           reason = Error}};
        {ok, #dns_rec{anlist = []}} ->
            {error, #error{qtype  = Type,
                           server = NSAddress,
                           domain = Domain,
                           reason = record_not_found}};
        {ok, Record = #dns_rec{anlist = [#dns_rr{type = RType}|_]}} ->
            {ok, #results{qtype   = Type,
                          rtype   = RType,
                          domain  = Domain,
                          server  = NSAddress,
                          answers = answers(Record#dns_rec.anlist)}}
    end.

-spec answers(list(#dns_rr{})|#dns_rr{}) -> list(string()).
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
