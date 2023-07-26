-module(handler_nameservers).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Nameservers = banker:nameservers(),
    Body = string:join(Nameservers, "\n"),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
