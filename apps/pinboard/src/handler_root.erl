-module(handler_root).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("/domains: ~p~n/nameservers: ~p~n",
                         [erlang:length(banker:domains()),
                          erlang:length(banker:nameservers())]),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
