-module(banker_ets).
-define(TABLE_NAME, centralbank).

-export([load/0,save/0,init/0,insert/3]).

init() ->
    ?TABLE_NAME = ets:new(?TABLE_NAME, [bag, protected, named_table]).

save() -> save(?TABLE_NAME, bag).
load() -> load(?TABLE_NAME, bag).

insert(_,_, [])             -> ok;
insert(Domain, Type, [H|T]) ->
    insert_if_new(Domain, Type, H),
    insert(Domain, Type, T).

insert_if_new(Domain, Type, Record) ->
    Results = ets:match(?TABLE_NAME, {Domain, '_', Type, Record}),
    insert_if_new(Domain, Type, Record, Results).

insert_if_new(Domain, Type, Record, []) ->
    Now = now_gregorian_seconds(),
    ets:insert(?TABLE_NAME, {Domain, Now, Type, Record});
insert_if_new(_,_,_,_)                  ->
    ok.

now_gregorian_seconds() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).

load(Name, Type) ->
    {ok, Dets} = dets:open_file(Name, [{type,Type}]),
    ets:delete_all_objects(Name),
    ets:from_dets(Name, Dets),
    dets:close(Dets).
save(Name, Type) ->
    {ok, Dets} = dets:open_file(Name, [{type,Type}]),
    ets:to_dets(Name, Dets),
    dets:close(Dets).
