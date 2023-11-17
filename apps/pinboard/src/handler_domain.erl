-module(handler_domain).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"application/json">>},
    Domain = cowboy_req:binding(domain, Req),
    Rows = banker:lookup(Domain),
    Body = json_format(Domain, Rows),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.

json_format(Domain, Rows)
  when erlang:is_list(Rows) ->
    Records = lists:map(fun ({Date,Type,Value})
                            -> #{date => Date, type => Type, value => Value}
                        end,
                        Rows),
    jsone:encode(#{domain => Domain, records => Records}).
