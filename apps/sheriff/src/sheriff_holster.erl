-module(sheriff_holster).
-behaviour(gen_server).
-export([async_lookup/2]).
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-define(SERVER, ?MODULE).
-record(state, {servers=[],timeout=1000}).

async_lookup(From, Domain) ->
    gen_server:cast(?SERVER, {lookup, From, Domain}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Timeout = sheriff_default:dns_timeout(),
    NSs = sheriff_default:dns_servers(),
    {ok, #state{servers=NSs,timeout=Timeout}}.

handle_cast({lookup, From, Domain}, #state{timeout=Timeout, servers=NSs}=State) ->
    NS = random_dns_server(NSs),
    erlang:spawn(sheriff_pony, express, [From, Domain, NS, Timeout]),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.

%% Internal Functions

random_dns_server(NSs) ->
    [{lists:nth(
        rand:uniform(erlang:length(NSs)),
        NSs), 53}].
