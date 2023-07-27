-module(banker_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link(DBConfig) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DBConfig]).

init([DBConfig]) ->
    {ok, {#{strategy  => one_for_one,
            intensity => 0,
            period    => 1},
          [#{id        => banker_vault,
             start     => {banker_vault,start_link,[DBConfig]}},
           #{id        => banker_atm,
             start     => {banker_atm,start_link,[]}}]}}.
