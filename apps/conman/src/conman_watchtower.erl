-module(conman_watchtower).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-export([is_connected/0
        ,is_disconnected/0
        ,connect/0
        ,disconnect/0
        ,get_disconnections/0]).

-define(SERVER, ?MODULE).

-type connection_status() :: 'connected' | 'disconnected'.
-type connection_event() :: {connection_status(), calendar:datetime()}.

-record(state, { status = 'disconnected' :: connection_status()
               , disconnections = 0 :: non_neg_integer()
               , events = [] :: [connection_event()]}).


-spec get_disconnections() -> non_neg_integer().
get_disconnections() -> gen_server:call(?MODULE, 'get_disconnections').

-spec connect() -> ok.
connect() -> gen_server:cast(?MODULE, 'connect').

-spec disconnect() -> ok.
disconnect() -> gen_server:cast(?MODULE, 'disconnect').

-spec is_connected() -> boolean().
is_connected() -> gen_server:call(?SERVER, 'is_connected').

-spec is_disconnected() -> boolean().
is_disconnected() -> gen_server:call(?SERVER, 'is_disconnected').


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.


handle_call('get_disconnections', _From, #state{disconnections = Disconnections} = State) ->
    {reply, Disconnections, State};
handle_call('is_connected', _From, #state{status=Status} = State) ->
    {reply, Status == 'connected', State};
handle_call('is_disconnected', _From, #state{status=Status} = State) ->
    {reply, Status == 'disconnected', State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast('connect', #state{ status = 'disconnected'
                             , events = Events} = State) ->
    {noreply, State#state{ status = 'connected'
                         , events = [{'connected', erlang:localtime()} | Events]}};
handle_cast('disconnect', #state{ status = 'connected'
                                , disconnections = Disconnections
                                , events = Events} = State) ->
    {noreply, State#state{ status = 'disconnected'
                         , events = [{'disconnected', erlang:localtime()} | Events]
                         , disconnections = Disconnections + 1}};
handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State)           -> {noreply, State}.
terminate(_Reason, _State)          -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status)         -> Status.
