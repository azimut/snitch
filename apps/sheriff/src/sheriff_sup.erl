-module(sheriff_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_child/1, start_child/2]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Domain) ->
    start_child(Domain, sheriff_default:dns_timeout()).
start_child(Domain, Timeout) ->
    supervisor:start_child(?SERVER, [Timeout, Domain]).

init([]) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity => 0,
                 period    => 1},
    ChildSpc = #{id        => holster,
                 start     => {sheriff_holster,start_link,[]},
                 restart   => permanent,
                 shutdown  => 2000,
                 type      => worker,
                 modules   => [sheriff_holster]},
    {ok, {SupFlags, [ChildSpc]}}.
