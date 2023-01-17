-module(banker).
-export([insert/5, insert_error/4]).
-include("header.hrl").

-spec insert(string(), inet:ip_address(), inet_res:dns_rr_type(), inet_res:dns_rr_type(), string()) -> ok.
insert(Domain, Ns, QType, RType, Result) ->
    banker_vault:insert(Domain, Ns, QType, RType, Result).

-spec insert_error(string(), inet_res:dns_rr_type(), inet:ip_address(), atom()) -> ok.
insert_error(Domain, Type, NS, ECode) ->
    banker_vault:insert_error(Domain, Type, NS, ECode).
