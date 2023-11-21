-module(handler_root).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Body = io_lib:format("/domains: ~p~n"
                         ++ "/nameservers: ~p~n"
                         ++ "/events: ~p~n~n"
                         ++ "No disconnections: ~p~n",
                         [banker:count_domains()
                         ,banker:count_nameservers()
                         ,banker:count_events()
                         ,conman:get_disconnections()]),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.
