-module(epgsql_pool_client).
-include("header.hrl").
-behaviour(ecpool_worker).
-export([connect/1, squery/1, equery/2]).

connect(Opts) ->
    Host = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    epgsql:connect(Host, Username, Password, Opts).

squery(Sql) ->
    squery(?POOL_NAME, Sql).
squery(Pool, Sql) ->
    ecpool:with_client(Pool, fun(Client) -> epgsql:squery(Client, Sql) end).

equery(Sql, Args) ->
    equery(?POOL_NAME, Sql, Args).
equery(Pool, Sql, Args) ->
    ecpool:with_client(Pool, fun(Client) -> epgsql:equery(Client, Sql, Args) end).
