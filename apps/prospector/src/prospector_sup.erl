%%%-------------------------------------------------------------------
%% @doc prospector top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(prospector_sup).

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
    ChildSpc = #{id        => prospector,
                 start     => {prospector_pickaxe, start_link, []},
                 restart   => permanent,
                 shutdown  => 5000,
                 type      => worker,
                 modules   => [prospector_pickaxe]},
    {ok, {SupFlags, [ChildSpc]}}.

%% internal functions
