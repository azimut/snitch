-module(sheriff_holster).
-behaviour(gen_server).
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-define(SERVER, ?MODULE).
-record(state, {timeout=1000}).
-include("header.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([Domain, Timeout]) ->
    process_flag(trap_exit, true),
    {ok, #state{timeout=Timeout}}.

handle_cast({lookup, From, Domain}, #state{timeout=Timeout}=State) ->
    NS = mattress:random_dns_server(),
    erlang:spawn(sheriff_revolver, shoot, [From,Domain,NS,Timeout]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({ok, NS, Domain, QType, {RType, RList}}, State) ->
    banker_vault:insert(ok, Domain, NS, QType, RType, RList),
    {noreply, State};
handle_info({error, NS, Domain, QType, Error}, State) ->
    banker_vault:insert(error, Domain, NS, QType, nil, Error),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.

%% Internal Functions
