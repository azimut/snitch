-module(sheriff_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 0,
                 period    => 1},
    ChildSpc = #{id        => holster,
                 start     => {sheriff_holster,start_link,[]},
                 restart   => permanent,
                 shutdown  => 1000,
                 type      => worker,
                 modules   => [sheriff_holster]},
    {ok, {SupFlags, [ChildSpc]}}.
