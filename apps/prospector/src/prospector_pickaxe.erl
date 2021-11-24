-module(prospector_pickaxe).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([add/1,del/1]).

-record(state, {domains=dict:new()}).

-define(SERVER, ?MODULE).
-define(TICK_SECONDS, 60).
-define(TICK_SECONDS_RANGE, 12*60*60).

add(Domain) -> gen_server:cast(?MODULE, {add, Domain}).
del(Domain) -> gen_server:cast(?MODULE, {del, Domain}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    schedule(tick, ?TICK_SECONDS),
    {ok, #state{}}.

handle_cast({add, RawDomain}, State) ->
    Domain = sanitize(RawDomain),
    OldDomains = State#state.domains,
    NewDomains = dict:store(Domain, 0, OldDomains),
    {noreply, #state{domains=NewDomains}};
handle_cast({del, RawDomain}, State) ->
    Domain = sanitize(RawDomain),
    NewDomains = dict:erase(Domain, State#state.domains),
    {noreply, #state{domains=NewDomains}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, State) ->
    schedule(tick, ?TICK_SECONDS),
    Old = State#state.domains,
    New = dict:map(fun tick_domain/2, Old),
    {noreply, #state{domains=New}};
handle_info({ok, NS, Domain, QType, {RType, RList}}, State) ->
    banker_vault:insert(ok, Domain, NS, QType, RType, RList),
    {noreply, State};
handle_info({error, NS, Domain, QType, Error}, State) ->
    banker_vault:insert(error, Domain, NS, QType, nil, Error),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

sanitize(Domain) ->
    Tmp1 = string:lowercase(Domain),
    Tmp2 = string:split(Tmp1, ".", all),
    Tmp3 = lists:filter(fun (X) -> length(X) > 0  end, Tmp2),
    string:join(Tmp3, ".").

schedule(Msg, Seconds) ->
    erlang:send_after(Seconds * 1000, erlang:self(), Msg).

tick_domain(Domain, Future) ->
    tick_domain(Domain, Future, now_gregorian_seconds()).
tick_domain(_Domain, Future, Now)
  when Now < Future ->
    Future;
tick_domain(Domain, Future, Now)
  when Now >= Future ->
    sheriff_holster:async_lookup(erlang:self(), Domain),
    future_gregorian_seconds().

now_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

future_gregorian_seconds() ->
    Now = now_gregorian_seconds(),
    Delta = rand:uniform(?TICK_SECONDS_RANGE),
    Now + Delta.
