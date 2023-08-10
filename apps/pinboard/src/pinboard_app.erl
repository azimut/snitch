-module(pinboard_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{'_', [{"/",               handler_root,        []},
                     {"/domains",        handler_domains,     []},
                     {"/nameservers",    handler_nameservers, []},
                     {"/domain/:domain", handler_domain,      []},
                     {"/latest",         handler_latest,      []}
                    ]}],
    Dispatch = cowboy_router:compile(Routes),
    Name = pinboard_http_listener,
    {ok, Port} = application:get_env(pinboard, port),
    Ports = [{port, Port}],
    Env = #{env => #{dispatch => Dispatch}},
    {ok, _} = cowboy:start_clear(Name, Ports, Env),
    pinboard_sup:start_link().

stop(_State) ->
    ok.
