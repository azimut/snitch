-module(sheriff_holster).
-behaviour(gen_server).
-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).
-export([lookup/1]).
-define(SERVER, ?MODULE).
-record(state, {}).
-include("header.hrl").


-spec lookup(Domain :: string()) -> ok.
lookup(Domain) -> gen_server:cast(?SERVER, {lookup, Domain}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_cast({lookup, Domain}, State) ->
    case conman:is_connected() of
        true ->
            NS = banker:random_nameserver(),
            Timeout = 5 * 1000,
            Arguments = [self(),Domain, {NS,53}, Timeout],
            proc_lib:spawn(sheriff_revolver, shoot, Arguments);
        false ->
            ok
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info({ok, #results{qtype=QType,rtype=RType,domain=Domain,server=NS,answers=Answers}}, State)
  when RType == 'cname'; RType == 'mx' ->
    lists:foreach(fun (Answer) ->
                          banker:insert(Domain, NS, QType, RType, Answer),
                          banker:add(Answer)
                  end,
                  Answers),
    {noreply, State};
handle_info({ok, #results{qtype=QType,rtype=RType,domain=Domain,server=NS,answers=Answers}}, State) ->
    lists:foreach(fun (Answer) -> banker:insert(Domain, NS, QType, RType, Answer) end, Answers),
    {noreply, State};
handle_info({error, #error{qtype=QType,server=NS,domain=Domain,reason='timeout'}}, State) ->
    conman:check_connectivity(),
    banker:insert_error(Domain, QType, NS, 'timeout'), % FIXME: timeout gets recorded, regardles of connection status at least once
    {noreply, State};
handle_info({error, #error{qtype=QType,server=NS,domain=Domain,reason=Reason}}, State) ->
    banker:insert_error(Domain, QType, NS, Reason),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.


handle_call(_Request, _From, State) -> {reply, ok, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) -> Status.
