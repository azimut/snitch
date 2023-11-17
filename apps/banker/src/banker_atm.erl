-module(banker_atm).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([random_nameserver/0,
         domains/0, count_domains/0,
         nameservers/0, count_nameservers/0,
         count_events/0,
         add/1]).

-define(SERVER, ?MODULE).
-define(CACHE_TIMEOUT, 3600).
-define(CACHE_STEP, 30).

-record(state, {timeout     = ?CACHE_TIMEOUT :: integer(),
                domains     = [] :: [string()],
                nevents     = 0 :: non_neg_integer(),
                nameservers = [] :: [inet:ip_address()]}).

-spec domains() -> [string()].
domains() ->
    {ok, Domains} = gen_server:call(?SERVER, domains),
    Domains.

-spec count_domains() -> non_neg_integer().
count_domains() ->
    {ok, Count} = gen_server:call(?SERVER, count_domains),
    Count.

-spec nameservers() -> [string()].
nameservers() ->
    {ok, Nameservers} = gen_server:call(?SERVER, nameservers),
    lists:map(fun inet:ntoa/1, Nameservers).

-spec count_nameservers() -> non_neg_integer().
count_nameservers() ->
    {ok, Count} = gen_server:call(?SERVER, count_nameservers),
    Count.

-spec count_events() -> non_neg_integer().
count_events() ->
    {ok, Count} = gen_server:call(?SERVER, count_events),
    Count.

-spec random_nameserver() -> inet:ip_address().
random_nameserver() ->
    {ok, NS} = gen_server:call(?SERVER, random_nameserver),
    NS.

-spec add(Domain :: string()) -> ok.
add(Domain) -> gen_server:cast(?MODULE, {add, Domain}).

%%-------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    schedule(tick, ?CACHE_TIMEOUT),
    {ok, new_state()}.


handle_call(count_events, _From, #state{nevents = Nevents}=State) ->
    {reply, {ok, Nevents}, State};
handle_call(count_domains, _From, #state{domains = Domains}=State) ->
    Count = erlang:length(Domains),
    {reply, {ok, Count}, State};
handle_call(count_nameservers, _From, #state{nameservers = Nameservers}=State) ->
    Count = erlang:length(Nameservers),
    {reply, {ok, Count}, State};
handle_call(domains, _From, #state{domains = Domains}=State) ->
    {reply, {ok, Domains}, State};
handle_call(nameservers, _From, #state{nameservers = Nameservers}=State) ->
    {reply, {ok, Nameservers}, State};
handle_call(random_nameserver, _From, #state{nameservers = Nameservers}=State) ->
    {reply, {ok, random_elt(Nameservers)}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.


handle_cast({add, RawDomain}, #state{domains = Domains}=State) ->
    Domain = sanitize(RawDomain),
    case lists:member(Domain, Domains) of
        false ->
            banker_vault:add(Domain),
            {noreply, State#state{domains = [Domain|Domains]}};
        true ->
            {noreply, State}
    end;
handle_cast(refresh_cache, _State) ->
    {noreply, new_state()};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(tick, #state{timeout = Timeout}=State)
  when Timeout < 0 ->
    refresh_cache(),
    schedule(tick, ?CACHE_STEP),
    {noreply, State#state{timeout = ?CACHE_TIMEOUT}};
handle_info(tick, #state{timeout = Timeout}=State) ->
    schedule(tick, ?CACHE_STEP),
    {noreply, State#state{timeout = Timeout - ?CACHE_STEP}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status)         -> Status.

%%------------------------------

-spec random_elt(list(any())) -> any().
random_elt(Xs) -> lists:nth(rand:uniform(erlang:length(Xs)), Xs).

-spec refresh_cache() -> ok.
refresh_cache() -> gen_server:cast(?SERVER, refresh_cache).

-spec schedule(atom(), integer()) -> reference().
schedule(Msg, Seconds) -> erlang:send_after(Seconds * 1000, erlang:self(), Msg).

-spec sanitize(string()) -> string().
sanitize(Domain) ->
    Tmp1 = string:lowercase(Domain),
    Tmp2 = string:split(Tmp1, ".", all),
    Tmp3 = lists:filter(fun (X) -> length(X) > 0  end, Tmp2),
    string:join(Tmp3, ".").

-spec new_state() -> #state{}.
new_state() ->
    #state{nameservers = banker_vault:nameservers()
          ,domains     = banker_vault:domains()
          ,nevents     = banker_vault:count_events()
          ,timeout     = ?CACHE_TIMEOUT}.
