-module(banker_ets).
-define(TABLE_NAME, centralbank).

-export([load/0,save/0,init/0,insert/3]).

init() ->
    ?TABLE_NAME = ets:new(?TABLE_NAME, [bag, protected, named_table]).

save() -> save(?TABLE_NAME, bag).
load() -> load(?TABLE_NAME, bag).

insert(_,_, [])             -> ok;
insert(Domain, Type, [H|T]) ->
    ets:insert(?TABLE_NAME, {Domain, Type, H}),
    insert(Domain, Type, T).

%% now_gregorian_seconds() ->
%%     calendar:datetime_to_gregorian_seconds(
%%       calendar:local_time()).

load(Name, Type) ->
    {ok, Dets} = dets:open_file(Name, [{type,Type}]),
    ets:delete_all_objects(Name),
    ets:from_dets(Name, Dets),
    dets:close(Dets).
save(Name, Type) ->
    {ok, Dets} = dets:open_file(Name, [{type,Type}]),
    ets:to_dets(Name, Dets),
    dets:close(Dets).
