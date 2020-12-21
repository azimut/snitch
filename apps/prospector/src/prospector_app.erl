%%%-------------------------------------------------------------------
%% @doc prospector public API
%% @end
%%%-------------------------------------------------------------------

-module(prospector_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    prospector_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
