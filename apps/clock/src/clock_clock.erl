-module(clock_clock).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([add/1, del/1, get_state/0]).

-record(state, {domains = dict:new()}).

-define(SERVER, ?MODULE).
-define(TICK_SECONDS, 60).
-define(TICK_SECONDS_RANGE, 12*60*60).

-spec add(Domain :: string()) -> ok.
add(Domain) -> gen_server:cast(?MODULE, {add, sanitize(Domain)}).

-spec del(Domain :: string()) -> ok.
del(Domain) -> gen_server:cast(?MODULE, {del, sanitize(Domain)}).

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
    NewDomains = dict:store(Domain, 0, Domains),
    {noreply, #state{domains = NewDomains}};
handle_cast({del, Domain}, #state{domains = Domains}) ->
    NewDomains = dict:erase(Domain, Domains),
    {noreply, #state{domains = NewDomains}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(tick, #state{domains = Old}) ->
    schedule(tick, ?TICK_SECONDS),
    New = dict:map(fun (K,V) -> tick_domain(K, V) end, Old),
    {noreply, #state{domains=New}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(get_state, _From, #state{domains = Domains}=State) ->
    {reply, {ok, dict:to_list(Domains)}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule(Msg, Seconds) ->
    erlang:send_after(Seconds * 1000, erlang:self(), Msg).

tick_domain(Domain, Future) ->
    case now_gregorian() < Future of
        true  -> Future;
        false -> sheriff:lookup(Domain),
                 future_gregorian()
    end.

-spec now_gregorian() -> non_neg_integer().
now_gregorian() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

-spec future_gregorian() -> non_neg_integer().
future_gregorian() ->
    now_gregorian() + rand:uniform(?TICK_SECONDS_RANGE).

-spec sanitize(string()|bitstring()) -> string().
sanitize(Domain) when is_bitstring(Domain) ->
    sanitize(binary:bin_to_list(Domain));
sanitize(Domain) ->
    Tmp1 = string:lowercase(Domain),
    Tmp2 = string:split(Tmp1, ".", all),
    Tmp3 = lists:filter(fun (X) -> length(X) > 0  end, Tmp2),
    string:join(Tmp3, ".").
