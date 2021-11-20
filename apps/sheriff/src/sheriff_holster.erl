-module(sheriff_holster).
-behaviour(gen_server).
-export([async_lookup/2,async_lookup/3,sync_lookup/2]).
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-define(SERVER, ?MODULE).
-record(state, {}).

sync_lookup(Domain, Type) ->
    gen_server:call(?MODULE, {lookup, Domain, Type}).

async_lookup(From, Domain)       ->
    gen_server:cast(?SERVER, {lookup, From, Domain}).
async_lookup(From, Domain, Type) ->
    gen_server:cast(?SERVER, {lookup, From, Domain, Type}).

%% Default Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({lookup, Domain, Type}, _From, State) ->
    Reply = revolver:lookup(Domain, Type),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({lookup, From, Domain}, State)       ->
    erlang:spawn(sheriff_pony, express, [From, Domain]),
    {noreply, State};
handle_cast({lookup, From, Domain, Type}, State) ->
    erlang:spawn(sheriff_pony, express, [From, Domain, Type]),
    {noreply, State};
handle_cast(_Request, State)                     ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.

