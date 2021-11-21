-module(banker_vault).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([insert/3]).

-define(SERVER, ?MODULE).
-record(state, {conn}).

insert(Domain, Type, Data) ->
    gen_server:cast(?SERVER, {insert, Domain, Type, Data}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, C} = epgsql:connect(
                #{
                  host => "127.0.0.1",
                  username => "autoaim",
                  password => "zaq12wsx",
                  database => "binance",
                  timeout => 4000
                 }),
    {ok, #state{conn=C}}.

handle_cast({insert, Domain, Type, [H,_]}, #state{conn=C}=State) ->
    Query = "INSERT INTO dns_what (name,rtype,data) VALUES ($1,$2,$3)",
    Parameters = [Domain, Type, H],
    {ok, 1} = epgsql:equery(C, Query, Parameters),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
format_status(_Opt, Status) ->
    Status.
