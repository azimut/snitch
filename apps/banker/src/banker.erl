-module(banker).

-export([insert/5,
         insert_error/4,
         nameservers/0,
         domains/0,
         random_nameserver/0,
         lookup/1
        ]).

-spec insert(string(), inet:ip_address(), inet_res:dns_rr_type(), inet_res:dns_rr_type(), string()) -> ok.
insert(Domain, Ns, QType, RType, Result) ->
    banker_vault:insert(Domain, Ns, QType, RType, Result).

-spec insert_error(string(), inet_res:dns_rr_type(), inet:ip_address(), atom()) -> ok.
insert_error(Domain, Type, NS, ECode) ->
    banker_vault:insert_error(Domain, Type, NS, ECode).

-spec random_nameserver() -> inet:ip_address().
random_nameserver() ->
    banker_atm:random_nameserver().

-spec nameservers() -> [string()].
nameservers()       -> banker_atm:nameservers().

-spec domains()     -> [string()].
domains()           -> banker_atm:domains().

%% TODO -spec lookup(Domain :: string()) -> [].
lookup(Domain) -> banker_vault:lookup(Domain).
