%%%-------------------------------------------------------------------
%% @doc snitch public API
%% @end
%%%-------------------------------------------------------------------


-module(snitch_app).

-behaviour(application).

-export([start/2, stop/1]).
-include("db.hrl").

start(_StartType, _StartArgs) ->
    application:set_env(locus, license_key, ?MMD),
    ok = locus:start_loader(asn, 'GeoLite2-ASN'),
    snitch_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
