-module(handler_domains).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Domains = banker:domains(),
    Res = cowboy_req:reply(200, Headers, join(<<"\n">>, Domains), Req),
    {ok, Res, State}.

%% https://gist.github.com/FNickRU/4daf8fb9afe5caaee7ebd35f398a8ac9
-spec join(Separator :: binary(), List :: [binary()]) -> binary().
join(_Separator, []) ->
    <<>>;
join(Separator, [H|T]) ->
    lists:foldl(fun (Value, Acc) -> <<Acc/binary, Separator/binary, Value/binary>> end,
                H, T).
