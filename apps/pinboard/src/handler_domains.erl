-module(handler_domains).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    ClockState = clock:state(),
    Lines = lists:map(fun ({Domain, Timeout})
                          -> unwords([Domain, human_time(Timeout)])
                      end,
                      ClockState),
    Body = unlines(Lines),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.

human_time(Seconds) ->
    {H, M, S} = calendar:seconds_to_time(Seconds),
    human_time(H, M, S).

human_time(0, 0, S) -> io_lib:format("~ps", [S]);
human_time(0, M, S) -> io_lib:format("~pm ~ps", [M, S]);
human_time(H, M, S) -> io_lib:format("~ph ~pm ~pm", [H, M, S]).

unlines(Lines) -> string:join(Lines, "\n").
unwords(Words) -> string:join(Words, " ").
