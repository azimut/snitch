-module(banker_vault).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([insert/6]).

-define(SERVER, ?MODULE).
-record(state, {conn}).

insert(Status, Domain, NS, QType, RType, RList) ->
    gen_server:cast(?SERVER, {insert, Status, Domain, NS, QType, RType, RList}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, C} = epgsql:connect(
                #{host     => "127.0.0.1",
                  username => "autoaim",
                  password => "zaq12wsx",
                  database => "binance",
                  timeout  => 4000}),
    {ok, #state{conn=C}}.

handle_cast({insert,_NS,_Status,_Domain,_QType,_RType, []}, State) ->
    {noreply, State};
handle_cast({insert, NS, Status, Domain, QType, RType, [H|T]}, State)
  when erlang:is_list(H) -> % head is a string
    handle_cast({insert, NS, Status, Domain, QType, RType, H}, State),
    handle_cast({insert, NS, Status, Domain, QType, RType, T}, State),
    {noreply, State};
handle_cast({insert, NS, Status, Domain, QType, RType, Result}, #state{conn=C}=State) ->
    Query = "INSERT INTO dns_data (domain_name,server,qtype,rtype,response,rcode) VALUES ($1,$2,$3,$4,$5,$6) ON CONFLICT DO NOTHING",
    Parameters = [Domain, NS, QType, RType, Result, Status],
    {ok, _} = epgsql:equery(C, Query, Parameters),
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
