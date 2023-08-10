-module(banker_vault).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([insert/5,
         insert_error/4,
         domains/0,
         nameservers/0,
         lookup/1,
         add/1,
         latest/1
        ]).
-define(SERVER, ?MODULE).
-record(state, {}).
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

-spec add(Domain :: string()) -> ok.
add(Domain) -> gen_server:cast(?MODULE, {add, Domain}).

-spec latest(Limit :: non_neg_integer())
            -> [{any(), string(), string(), string()}].
latest(Limit) ->
    {ok, Rows} = gen_server:call(?MODULE, {latest, Limit}),
    Rows.

%% ========================================

start_link(DBConfig) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [DBConfig], []).

init([DBConfig]) ->
    process_flag(trap_exit, true),
    ecpool:start_pool(?POOL_NAME, epgsql_pool_client, DBConfig),
    {ok, #state{}}.

handle_cast({add, Domain}, State) ->
    SQL = "INSERT INTO domains (addr) VALUES ($1) ON CONFLICT DO NOTHING",
    {ok, _} = epgsql_pool_client:equery(SQL, [Domain]),
    {noreply, State};
handle_cast({ok, #dns_data{}=D}, State) ->
    SQL = "INSERT INTO dns_data (domain_name,dns,qtype,rtype,response)" ++
        "  VALUES ($1,$2,$3,$4,$5)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [D#dns_data.domain,
                  D#dns_data.ns,
                  erlang:atom_to_list(D#dns_data.qtype),
                  erlang:atom_to_list(D#dns_data.rtype),
                  D#dns_data.result],
    {ok, _} = epgsql_pool_client:equery(SQL, Parameters),
    {noreply, State};
handle_cast({error, #dns_error{rerror = ehostunreach, ns = NS}}, State) ->
    TSQL = "UPDATE nameservers SET enabled = false WHERE ip = $1",
    {ok, _} = epgsql_pool_client:equery(TSQL, [NS]),
    {noreply, State};
handle_cast({error, #dns_error{rerror = timeout}=E}, State) ->
    TSQL = "UPDATE nameservers SET timeouts = timeouts + 1 WHERE ip = $1",
    {ok, _} = epgsql_pool_client:equery(TSQL, [E#dns_error.ns]),
    SQL = "INSERT INTO dns_error (domain_name,qtype,rerror,dns)" ++
        "  VALUES ($1,$2,$3,$4)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [E#dns_error.domain,
                  erlang:atom_to_list(E#dns_error.qtype),
                  erlang:atom_to_list(E#dns_error.rerror),
                  E#dns_error.ns],
    {ok, _} = epgsql_pool_client:equery(SQL, Parameters),
    {noreply, State};
handle_cast({error, #dns_error{}=E}, State) ->
    SQL = "INSERT INTO dns_error (domain_name,qtype,rerror,dns)" ++
        "       VALUES ($1,$2,$3,$4)" ++
        "  ON CONFLICT DO NOTHING",
    Parameters = [E#dns_error.domain,
                  erlang:atom_to_list(E#dns_error.qtype),
                  erlang:atom_to_list(E#dns_error.rerror),
                  E#dns_error.ns],
    {ok, _} = epgsql_pool_client:equery(SQL, Parameters),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% TODO: terminate pool
terminate(_Reason, _State) -> ok.

handle_call({latest, Limit}, _From, State) ->
    SQL = "SELECT created, domain_name, rtype, response" ++
        "    FROM dns_data" ++
        "   WHERE rtype=qtype " ++
        "ORDER BY created DESC" ++
        "   LIMIT $1",
    {ok, _Cols, Rows} = epgsql_pool_client:equery(SQL, [Limit]),
    Results = lists:map(fun ({Created, Domain, Type, Response}) ->
                                {Created,
                                 erlang:binary_to_list(Domain),
                                 erlang:binary_to_list(Type),
                                 erlang:binary_to_list(Response)}
                        end,
                        Rows),
    {reply, {ok, Results}, State};
handle_call({lookup, Domain}, _From, State) ->
    SQL = "SELECT created, qtype, response" ++
        "    FROM dns_data" ++
        "   WHERE domain_name = $1 " ++
        "ORDER BY created DESC",
    {ok, _Cols, Rows} = epgsql_pool_client:equery(SQL, [Domain]),
    {reply, Rows, State};
handle_call(domains, _From, State) ->
    SQL = "SELECT addr FROM domains",
    {ok, _Cols, Rows} = epgsql_pool_client:equery(SQL, []),
    Domain = lists:map(fun ({X}) -> binary:bin_to_list(X) end, Rows),
    {reply, {ok, Domain}, State};
handle_call(nameservers, _From, State) ->
    SQL = "SELECT ip FROM nameservers WHERE timeouts = 0 AND enabled",
    {ok, _Cols, Rows} = epgsql_pool_client:equery(SQL, []),
    IPs = lists:map(fun ({X}) -> X end, Rows),
    {reply, {ok, IPs}, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.
