%%%-------------------------------------------------------------------
%%% @author sendai <sendai@localhost.localdomain>
%%% @copyright (C) 2020, sendai
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2020 by sendai <sendai@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(prospector_pickaxe).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([list/0]).
-export([add/1,del/1]).

-define(SERVER, ?MODULE).
-define(TICK_SECONDS, 60).
-record(state, {domains=dict:new()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    process_flag(trap_exit, true),
    %% load dict from unique list of domains on ETS
    schedule(tick, ?TICK_SECONDS),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call(list, _From, State) ->
    Reply = dict:to_list(State#state.domains),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

list() -> gen_server:call(?MODULE, list).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast({add, RawDomain}, State) ->
    Domain = sanitize(RawDomain),
    NewDomains = add_if_missing(Domain, State#state.domains),
    {noreply, #state{domains=NewDomains}};
handle_cast({del, RawDomain}, State) ->
    Domain = sanitize(RawDomain),
    NewDomains = dict:erase(Domain, State#state.domains),
    {noreply, #state{domains=NewDomains}};
handle_cast(_Request, State) ->
    {noreply, State}.

add(Domain) -> gen_server:cast(?MODULE, {add, Domain}).
del(Domain) -> gen_server:cast(?MODULE, {del, Domain}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(tick, State) ->
    schedule(tick, ?TICK_SECONDS),
    Old = State#state.domains,
    New = dict:map(fun tick_domain/2, Old),
    {noreply, #state{domains=New}};
handle_info({Status, Data, Domain, Type}, State) -> % lookup reply
    banker_vault:store(Status, Domain, Type, Data),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sanitize(Domain) ->
    string:lowercase(Domain).

add_if_missing(Domain, Dict) ->
    add_if_missing(Domain, Dict, not dict:is_key(Domain, Dict)).

add_if_missing(Domain, Dict, true) -> dict:store(Domain, 0, Dict);
add_if_missing(_     , Dict, _)    -> Dict.

schedule(Msg, Seconds) ->
    erlang:send_after(Seconds * 1000, erlang:self(), Msg).

tick_domain(Domain, Gregorian) ->
    tick_domain(Domain, Gregorian, now_gregorian_seconds()).
tick_domain(Domain, Gregorian, Now)
  when Now > Gregorian ->
    sheriff_holster:async_lookup(erlang:self(), Domain),
    next_gregorian_seconds();
tick_domain(_, Gregorian, Now)
  when Now < Gregorian ->
    Gregorian.

now_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

next_gregorian_seconds() ->
    Now = now_gregorian_seconds(),
    Next = rand:uniform(12 * 60 * 60),
    Now + Next.
