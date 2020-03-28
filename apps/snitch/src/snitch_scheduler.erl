%%%-------------------------------------------------------------------
%%% @author azimut <azimut.github@protonmail.com>
%%% @copyright (C) 2020, azimut
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2020 by azimut <azimut.github@protonmail.com>
%%%-------------------------------------------------------------------
-module(snitch_scheduler).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-include("db.hrl").
-record(state, {domains_dict=dict:new()}).

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
          {ok  , State  :: term(), Timeout :: timeout()} |
          {ok  , State  :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?TABLE_NAME, [bag, public, named_table]),
    snitch_disk:load(),
    Dict = snitch_file:load(),
    schedule(tick, ?INIT_TICK),
    schedule(checkpoint, ?INIT_CHECKPOINT),
    {ok, #state{domains_dict=Dict}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply   , Reply    :: term(), NewState :: term()} |
          {reply   , Reply    :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply   , Reply    :: term(), NewState :: term(), hibernate} |
          {noreply , NewState :: term()} |
          {noreply , NewState :: term(), Timeout :: timeout()} |
          {noreply , NewState :: term(), hibernate} |
          {stop    , Reason   :: term(), Reply :: term(), NewState :: term()} |
          {stop    , Reason   :: term(), NewState :: term()}.

handle_call({get, Domain}, _From, State) ->
    Domains = State#state.domains_dict,
    Time = dict:fetch(Domain, Domains),
    {reply, Time, State};
handle_call(all, _From, State) ->
    Domains = State#state.domains_dict,
    All = dict:fetch_keys(Domains),
    {reply, All, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
          {stop   , Reason   :: term(), NewState :: term()}.

handle_cast({add, RawDomain}, State) ->
    Time = snitch_tempo:get_future_gregorian(),
    Domain = string:lowercase(RawDomain),
    Domains = State#state.domains_dict,
    NewDomains = dict:store(Domain, Time, Domains),
    {noreply, State#state{domains_dict=NewDomains}};
handle_cast(reload, State) ->
    NewState = snitch_file:load(),
    {noreply, State#state{domains_dict=NewState}};
handle_cast(checkpoint, State) ->
    snitch_disk:save(),
    {noreply, State};
handle_cast(Request, State) ->
    io:format("Unexpected _cast: ~p~n", [Request]),
    {noreply, State}.

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
    schedule(tick),
    Dict = State#state.domains_dict,
    NewDict = dict:map(fun process_domain/2, Dict),
    {noreply, State#state{domains_dict=NewDict}};
handle_info(checkpoint, State) ->
    schedule(checkpoint, ?DEFAULT_CHECKPOINT),
    snitch_disk:save(),
    {noreply, State};
handle_info(Info, State) ->
    io:format("Unexpected _info: ~p~n", [Info]),
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
    snitch_disk:save(),
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

schedule(Msg, Seconds) ->
    erlang:send_after(Seconds * 1000, erlang:self(), Msg).

schedule(Msg) ->
    schedule(Msg, ?DEFAULT_TICK).

query(Domain) ->
    erlang:spawn(snitch_parser, query_all, [Domain]),
    snitch_tempo:get_future_gregorian().

process_domain(Domain, Gregorian) ->
    case snitch_tempo:has_gregorian_passed(Gregorian) of
        true  -> query(Domain);
        false -> Gregorian
    end.


