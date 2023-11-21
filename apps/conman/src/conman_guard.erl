-module(conman_guard).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL_SECONDS, 10 * 60).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    schedule('tick', 0),
    {ok, #state{}}.


handle_cast('check_connectivity', State) ->
    case inet_res:gethostbyname("google.com") of
        {ok, _}    -> conman_watchtower:connect();
        {error, _} -> conman_watchtower:disconnect()
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info('tick', State) ->
    schedule('tick', ?TICK_INTERVAL_SECONDS),
    check_connectivity(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status)         -> Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec schedule(Msg :: atom(), Seconds :: non_neg_integer()) -> reference().
schedule(Msg, Seconds) ->
    erlang:send_after(timer:seconds(Seconds), erlang:self(), Msg).

-spec check_connectivity() -> ok.
check_connectivity() -> gen_server:cast(?MODULE, 'check_connectivity').
