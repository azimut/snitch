-module(conman_guard).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([check_connectivity/0]).

-define(SERVER, ?MODULE).

-record(state, {}).


-spec check_connectivity() -> ok.
check_connectivity() -> gen_server:cast(?MODULE, 'check_connectivity'). % FIXME: rate limit check_connectivity


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    check_connectivity(),
    {ok, #state{}}.


handle_cast('check_connectivity', State) ->
    logger:notice("checking internet connectivity..."),
    case inet_res:gethostbyname("google.com") of
        {ok, _}    -> conman_watchtower:connect();
        {error, _} -> conman_watchtower:disconnect()
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State)           -> {noreply, State}.
handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status)         -> Status.
