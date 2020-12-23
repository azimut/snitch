%%%-------------------------------------------------------------------
%% @doc snitch public API
%% @end
%%%-------------------------------------------------------------------

-module(snitch_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    snitch_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
