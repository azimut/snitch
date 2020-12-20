%%%-------------------------------------------------------------------
%% @doc sheriff public API
%% @end
%%%-------------------------------------------------------------------

-module(sheriff_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sheriff_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
