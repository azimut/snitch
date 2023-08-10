-module(handler_latest).
-export([init/2]).
-define(NUMBER_OF_ROWS, 20).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    RawRows = banker:latest(?NUMBER_OF_ROWS),
    Rows = lists:map(fun ({_, Domain, Type, Result}) ->
                             io_lib:format("~s ~s ~s", [Domain, Type, Result])
                     end,
                     RawRows),
    Body = string:join(Rows, "\n"),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
