-module(banker_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, DBConfig} = application:get_env(banker, db_config),
    banker_sup:start_link(DBConfig).

stop(_State) ->
    ok.
