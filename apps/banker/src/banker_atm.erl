-module(banker_atm).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([random_nameserver/0,
         domains/0,
         nameservers/0]).

-define(SERVER, ?MODULE).
-define(CACHE_TIMEOUT, 3600).
-define(CACHE_STEP, 30).

-record(state, {timeout = ?CACHE_TIMEOUT :: integer(),
                domains = [] :: list(string()),
                nameservers = [] :: list(inet:ip_address())}).

-spec domains() -> list(string()).
domains() ->
    {ok, Domains} = gen_server:call(?SERVER, domains),
    Domains.

-spec nameservers() -> list(inet:ip_address()).
nameservers() ->
    {ok, Nameservers} = gen_server:call(?SERVER, nameservers),
    Nameservers.

-spec random_nameserver() -> inet:ip_address().
random_nameserver() ->
    {ok, NS} = gen_server:call(?SERVER, random_nameserver),
    NS.

%%-------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    tick(),
    Domains = banker_vault:domains(),
    NSs = banker_vault:nameservers(),
    {ok, #state{domains = Domains, nameservers = NSs, timeout = ?CACHE_TIMEOUT}}.

handle_call(domains, _From, #state{domains = Domains}=State) ->
    {reply, {ok, Domains}, State};
handle_call(nameservers, _From, #state{nameservers = Nameservers}=State) ->
    {reply, {ok, Nameservers}, State};
handle_call(random_nameserver, _From, #state{nameservers=Nameservers}=State) ->
    {reply, {ok, random_elt(Nameservers)}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(refresh_cache, State) ->
    NSs = banker_vault:nameservers(),
    Domains = banker_vault:domains(),
    {noreply, State#state{nameservers = NSs, domains = Domains}};
handle_cast(_Request, State)        -> {noreply, State}.

handle_info(tick, #state{timeout = Timeout}=State)
  when Timeout < 0 ->
    refresh_cache(),
    tick(),
    {noreply, State#state{timeout = ?CACHE_TIMEOUT}};
handle_info(tick, #state{timeout = Timeout}=State) ->
    tick(),
    {noreply, State#state{timeout = Timeout - ?CACHE_STEP}};
handle_info(_Info, State)           -> {noreply, State}.

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

-spec tick() -> reference().
tick() -> schedule(tick, ?CACHE_STEP).
