-module(mattress).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).
-export([add_domain/1, del_domain/1, add_server/1, del_server/1, get_domains/0, random_dns_server/0]).
-define(SERVER, ?MODULE).
-define(DOMAIN_FILE, "/home/sendai/domains.txt").
-define(DNS_SERVERS_FILE, "/home/sendai/dns_servers.txt").
-record(state, {dns_servers = [] :: list(inet:ip_address()),
                domains = [] :: list(string())}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_domain(string()) -> ok.
-spec del_domain(string()) -> ok.
add_domain(Domain) -> gen_server:cast(?SERVER, {add_domain, Domain}).
del_domain(Domain) -> gen_server:cast(?SERVER, {del_domain, Domain}).

-spec add_server(string()) -> ok.
-spec del_server(string()) -> ok.
add_server(Addr) -> gen_server:cast(?SERVER, {add_server, parse_address(Addr)}).
del_server(Addr) -> gen_server:cast(?SERVER, {del_server, parse_address(Addr)}).

-spec get_domains() -> list(string()).
get_domains() ->
    {ok, Domains} = gen_server:call(?SERVER, domains),
    Domains.

-spec random_dns_server() -> inet:ip_address().
random_dns_server() ->
    {ok, NS} = gen_server:call(?SERVER, random_dns_server),
    NS.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    State = load(),
    lists:foreach(fun (Domain) -> clock:add(Domain) end, State#state.domains),
    {ok, State}.

handle_call(domains, _From, State) ->
    {reply, {ok, State#state.domains}, State};
handle_call(random_dns_server, _From, #state{dns_servers = DNSServers}=State) ->
    SomeServer = lists:nth(rand:uniform(erlang:length(DNSServers)), DNSServers),
    {reply, {ok, SomeServer}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_domain, Domain}, #state{domains = Domains}=State) ->
    clock:add(Domain),
    {noreply, State#state{domains = [Domain|Domains]}};
handle_cast({del_domain, Domain}, #state{domains = Domains}=State) ->
    clock:del(Domain),
    {noreply, State#state{domains = lists:delete(Domain, Domains)}};
handle_cast({add_server, IPAddress}, #state{dns_servers = DnsServers}=State) ->
    {noreply, State#state{dns_servers = [IPAddress|DnsServers]}};
handle_cast({del_server, IPAddress}, #state{dns_servers = DnsServers}=State) ->
    {noreply, State#state{dns_servers = lists:delete(IPAddress, DnsServers)}};
handle_cast(_Request, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    lists:foreach(fun (Domain) -> clock:del(Domain) end, State#state.domains),
    save(State),
    ok.

format_status(_Opt, Status) -> Status.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info(_Info, State) -> {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec parse_address(string()) -> inet:ip_address().
parse_address(Addr) ->
    {ok, IPAddress} = inet:parse_address(Addr),
    IPAddress.

-spec file_lines(string()) -> list(string()).
file_lines(Filename) ->
    {ok, BinaryText} = file:read_file(Filename),
    BinaryLines = binary:split(BinaryText, [<<"\n">>], [trim_all, global]),
    Lines = lists:map(fun binary_to_list/1, BinaryLines),
    Lines.

-spec load() -> #state{}.
load() ->
    #state{dns_servers = lists:map(fun parse_address/1, file_lines(?DNS_SERVERS_FILE)),
           domains = file_lines(?DOMAIN_FILE)}.

-spec save(#state{}) -> ok.
save(#state{domains = Domains, dns_servers = DNSServers}) ->
    IPs = lists:map(fun inet:ntoa/1, DNSServers),
    ok = file:write_file(?DNS_SERVERS_FILE, [string:join(IPs, "\n")]),
    ok = file:write_file(?DOMAIN_FILE, [string:join(Domains, "\n")]),
    ok.
