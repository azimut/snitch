-module(clock_clock).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([get_state/0]).

-record(state, {domains = dict:new()}).

-define(SERVER, ?MODULE).
-define(TICK_SECONDS, 10).
-define(TICK_SECONDS_MIN, 60*5).
-define(TICK_SECONDS_MAX, 60*60*8).

-spec get_state() -> [{string(), non_neg_integer()}].
get_state() ->
    {ok, State} = gen_server:call(?MODULE, get_state),
    State.

%% ==============================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    schedule(tick, ?TICK_SECONDS),
    Domains = banker:domains(),
    lists:foreach(fun add/1, Domains),
    {ok, #state{}}.

handle_cast({add, Domain}, #state{domains = Domains}) ->
    NewDomains = dict:store(Domain, next_timeout(), Domains),
    {noreply, #state{domains = NewDomains}};
handle_cast({del, Domain}, #state{domains = Domains}) ->
    NewDomains = dict:erase(Domain, Domains),
    {noreply, #state{domains = NewDomains}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, #state{domains = Old}) ->
    schedule(tick, ?TICK_SECONDS),
    New = dict:map(fun tick_domain/2, Old),
    {noreply, #state{domains = New}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_state, _From, #state{domains = Domains}=State) ->
    Ds = dict:to_list(Domains),
    SortedDs = lists:sort(fun ({_,T1}, {_,T2}) -> T1 < T2 end, Ds),
    {reply, {ok, SortedDs}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec schedule(Msg :: atom(), Seconds :: non_neg_integer()) -> reference().
schedule(Msg, Seconds) ->
    erlang:send_after(timer:seconds(Seconds), erlang:self(), Msg).

-spec tick_domain(Domain :: string(), Timeout :: non_neg_integer()) -> non_neg_integer().
tick_domain(Domain, Timeout) ->
    case Timeout > 0 of
        true  -> Timeout - ?TICK_SECONDS;
        false -> sheriff:lookup(Domain),% !!
                 next_timeout()
    end.

-spec next_timeout() -> non_neg_integer().
next_timeout() -> random_between(?TICK_SECONDS_MIN, ?TICK_SECONDS_MAX).

-spec random_between(Min :: non_neg_integer(), Max :: non_neg_integer()) -> non_neg_integer().
random_between(Min, Max)
  when Max > Min, Max > 0, Min > 0 ->
    rand:uniform(Max - Min + 1) + Min.

-spec add(Domain :: string()) -> ok.
add(Domain) -> gen_server:cast(?MODULE, {add, Domain}).
