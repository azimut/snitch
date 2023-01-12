-module(banker).
-export([insert/6]).
-include("header.hrl").

-spec insert(atom(), string(), string(), string(), string(), string()) -> atom().
insert(Status, Domain, Ns, Qry, Ans, Result) ->
    DNSData = #dns_data{status = Status,
                        domain = Domain,
                        ns = Ns,
                        qry = Qry,
                        ans = Ans,
                        result = Result},
    gen_server:cast(banker_vault, {insert, DNSData}).
