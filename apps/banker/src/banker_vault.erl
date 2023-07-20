-module(banker_vault).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([insert/5,insert_error/4]).
-define(SERVER, ?MODULE).
-record(state, {conn}).
-include("header.hrl").

-spec insert(string(), inet:ip_address(), inet_res:dns_rr_type(), inet_res:dns_rr_type(), string()) -> ok.
insert(Domain, Ns, QType, RType, Result) ->
    Data = #dns_data{domain = Domain, qtype = QType, rtype = RType, ns = Ns, result = Result},
    gen_server:cast(?SERVER, {ok, Data}).

-spec insert_error(string(), inet_res:dns_rr_type(), inet:ip_address(), atom()) -> ok.
insert_error(Domain, Type, NS, ECode) ->
    Error = #dns_error{domain = Domain, qtype = Type, ns = NS, rerror = ECode},
    gen_server:cast(?SERVER, {error, Error}).

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

handle_cast({ok, #dns_data{}=D}, #state{conn=Conn}=State) ->
    SQL = "INSERT INTO dns_data (domain_name,dns,qtype,rtype,response)" ++
        "  VALUES ($1,$2,$3,$4,$5)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [D#dns_data.domain,
                  D#dns_data.ns,
                  erlang:atom_to_list(D#dns_data.qtype),
                  erlang:atom_to_list(D#dns_data.rtype),
                  D#dns_data.result],
    {ok, _} = epgsql:equery(Conn, SQL, Parameters),
    {noreply, State};
handle_cast({error, #dns_error{}=E}, #state{conn=Conn}=State) ->
    SQL = "INSERT INTO dns_error (domain_name,qtype,rerror,dns)" ++
        "  VALUES ($1,$2,$3,$4)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [E#dns_error.domain,
                  erlang:atom_to_list(E#dns_error.qtype),
                  erlang:atom_to_list(E#dns_error.rerror),
                  E#dns_error.ns],
    {ok, _} = epgsql:equery(Conn, SQL, Parameters),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    epgsql:close(Conn).

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.
