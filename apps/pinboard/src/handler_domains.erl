-module(handler_domains).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    ClockState = clock:state(),
    Lines = lists:map(fun ({Domain, Timeout}) ->
                              string:join([Domain,
                                           erlang:integer_to_list(Timeout)], " ")
                      end,
                      ClockState),
    Body = string:join(Lines, "\n"),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
