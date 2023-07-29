-module(banker_vault).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([insert/5,
         insert_error/4,
         domains/0,
         nameservers/0,
         lookup/1]).
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

-spec nameservers() -> [inet:ip_address()].
nameservers() ->
    {ok, NSs} = gen_server:call(?SERVER, nameservers),
    NSs.

-spec domains() -> [string()].
domains() ->
    {ok, Domains} = gen_server:call(?SERVER, domains),
    Domains.

%% TODO -spec lookup(Domain :: string()) -> [].
lookup(Domain) ->
    gen_server:call(?SERVER, {lookup, Domain}).

%% ========================================

start_link(DBConfig) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBConfig], []).

init([DBConfig]) ->
    process_flag(trap_exit, true),
    {ok, C} = epgsql:connect(
                DBConfig#{application_name => "snitch",
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

handle_call({lookup, Domain}, _From, #state{conn = Conn}=State) ->
    SQL = "SELECT created, qtype, response FROM dns_data WHERE domain_name = $1" ++
        " ORDER BY created DESC",
    {ok, _Cols, Rows} = epgsql:equery(Conn, SQL, [Domain]),
    {reply, Rows, State};
handle_call(domains, _From, #state{conn=Conn}=State) ->
    SQL = "SELECT addr FROM domains",
    {ok, _Cols, Rows} = epgsql:equery(Conn, SQL, []),
    Domain = lists:map(fun ({X}) -> X end, Rows),
    {reply, {ok, Domain}, State};
handle_call(nameservers, _From, #state{conn=Conn}=State) ->
    SQL = "SELECT ip FROM nameservers",
    {ok, _Cols, Rows} = epgsql:equery(Conn, SQL, []),
    IPs = lists:map(fun ({X}) -> X end, Rows),
    {reply, {ok, IPs}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.
