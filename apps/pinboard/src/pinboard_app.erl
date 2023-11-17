-module(pinboard_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{'_', [{"/",                handler_root,        []},
                     {"/domains",         handler_domains,     []},
                     {"/domains/:domain", handler_domain,      []},
                     {"/nameservers",     handler_nameservers, []},
                     {"/events",          handler_events,      []}
                    ]}],
    Dispatch = cowboy_router:compile(Routes),
    {ok, Port} = application:get_env(pinboard, port),
    {ok, _} = cowboy:start_clear(
                pinboard_http_listener,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}
               ),
    logger:notice("Listening at http://127.0.0.1:~p~n", [Port]),
    pinboard_sup:start_link().

stop(_State) ->
    ok.
