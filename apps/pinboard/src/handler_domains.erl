-module(handler_domains).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    ClockState = clock:state(),
    Lines = lists:map(fun ({Domain, Timeout}) ->
                              string:join([Domain, human_time(Timeout)], " ")
                      end,
                      ClockState),
    Body = string:join(Lines, "\n"),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.

human_time(Seconds) ->
    {H, M, S} = calendar:seconds_to_time(Seconds),
    human_time(H, M, S).

human_time(0, 0, S) -> io_lib:format("~ps", [S]);
human_time(0, M, S) -> io_lib:format("~pm ~ps", [M, S]);
human_time(H, M, S) -> io_lib:format("~ph ~pm ~pm", [H, M, S]).
