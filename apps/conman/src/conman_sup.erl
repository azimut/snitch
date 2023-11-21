-module(conman_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, {#{strategy  => one_for_all,
            intensity => 0,
            period    => 1},
          [#{id    => conman_watchtower,
             start => {conman_watchtower,start_link,[]}},
           #{id    => conman_guard,
             start => {conman_guard,start_link,[]}}
          ]}}.
