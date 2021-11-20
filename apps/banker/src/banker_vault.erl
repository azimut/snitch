-module(banker_vault).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([store/4]).

-define(CHECKPOINT_SECONDS, 5 * 60).
-define(SERVER, ?MODULE).

-record(state, {}).

store(Status, Domain, Type, Data) ->
    gen_server:cast(?MODULE, {store, Status, Domain, Type, Data}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    banker_ets:init(),
    banker_ets:load(),
    prospector_pickaxe:load(),
    schedule(checkpoint, ?CHECKPOINT_SECONDS),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({store, error, Domain, Type, timeout}, State)       ->
    io:format("TIMEOUT: ~s ~s\n", [Domain, Type]),
    {noreply, State};
handle_cast({store, error, Domain, Type, Error}, State) ->
    io:format("ERROR: ~s ~s ~p\n", [Domain, Type, Error]),
    banker_sql:insert(Domain, Type, Error),
    {noreply, State};
handle_cast({store, ok, Domain, Type, Data}, State)     ->
    io:format("OK: ~s ~s ~p\n", [Domain, Type,Data]),
    banker_sql:insert(Domain, Type, Data),
    banker_ets:insert(Domain, Type, Data),
    {noreply, State};
handle_cast(_Request, State)                            ->
    {noreply, State}.

handle_info(checkpoint, State) ->
    schedule(checkpoint, ?CHECKPOINT_SECONDS),
    banker_ets:save(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule(Msg, Seconds) ->
    erlang:send_after(Seconds * 1000, erlang:self(), Msg).

