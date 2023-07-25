-module(handler_domains).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Lines = lists:map(fun ({Domain, Timeout}) ->
                              string:join([Domain, erlang:integer_to_list(Timeout)], " ")
                      end,
                      clock:state()),
    Body = string:join(Lines, "\n"),
    Res = cowboy_req:reply(200, Headers, binary:list_to_bin(Body), Req),
    {ok, Res, State}.

%% https://gist.github.com/FNickRU/4daf8fb9afe5caaee7ebd35f398a8ac9
-spec join(Separator :: binary(), List :: [binary()]) -> binary().
join(_Separator, []) ->
    <<>>;
join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end,
                H, T).
