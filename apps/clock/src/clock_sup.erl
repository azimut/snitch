-module(clock_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 0,
                 period    => 1},
    ChildSpc = #{id        => clock,
                 start     => {clock_clock,start_link,[]},
                 restart   => permanent,
                 shutdown  => 2000,
                 type      => worker,
                 modules   => [clock_clock]},
    {ok, {SupFlags, [ChildSpc]}}.
