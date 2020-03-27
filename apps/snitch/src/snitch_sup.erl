%%%-------------------------------------------------------------------
%% @doc snitch top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(snitch_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy  => one_for_all,
                 intensity => 2,
                 period    => 1},
    ChildSpecs = #{id       => calendario,
                   start    => {snitch_scheduler, start_link, []},
                   restart  => permanent,
                   shutdown => 5000,
                   type     => worker,
                   modules  => [snitch_scheduler]},
    {ok, {SupFlags, [ChildSpecs]}}.

%% internal functions
