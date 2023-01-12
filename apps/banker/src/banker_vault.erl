-module(banker_vault).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-define(SERVER, ?MODULE).
-record(state, {conn}).
-include("header.hrl").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, C} = epgsql:connect(
                #{host             => "127.0.0.1",
                  username         => "autoaim",
                  password         => "zaq12wsx",
                  database         => "binance",
                  application_name => "snitch",
                  timeout          => 4 * 1000}),
    {ok, #state{conn=C}}.

handle_cast({insert, #dns_data{}=D}, #state{conn=C}=State) ->
    SQL = "INSERT INTO dns_data (domain_name,server,qtype,rtype,response,rcode)" ++
        "  VALUES ($1,$2,$3,$4,$5,$6)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [D#dns_data.domain,
                  D#dns_data.ns,
                  D#dns_data.qry,
                  D#dns_data.ans,
                  D#dns_data.result,
                  D#dns_data.status],
    {ok, _} = epgsql:equery(C, SQL, Parameters),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.
