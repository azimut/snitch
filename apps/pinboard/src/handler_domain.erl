-module(handler_domain).
-export([init/2]).

init(Req, State) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    Domain = cowboy_req:binding(domain, Req),
    Rows = banker:lookup(Domain),
    Body = format(Rows),
    Res = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.

format([])   -> "No result.";
format(Rows) -> lists:foldl(fun row_folder/2, "", Rows).

row_folder({_Date, Type, Value}, Acc) ->
    Acc ++ io_lib:format("~s ~s~n", [Type, Value]).
