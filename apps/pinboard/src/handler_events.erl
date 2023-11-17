-module(handler_events).
-export([init/2]).
-define(NUMBER_OF_ROWS, 20).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    RawRows = banker:events(?NUMBER_OF_ROWS),
    Rows = lists:map(fun ({{{Y,M,D},_}, Domain, Type, Result}) ->
                             io_lib:format("~p/~p/~p ~s ~s ~s",
                                           [Y, M, D, Domain, Type, Result])
                     end,
                     RawRows),
    Body = string:join(Rows, "\n"),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
