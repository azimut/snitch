-module(banker_ets).
-define(TABLE_NAME, centralbank).

-export([load/0,save/0,init/0]).

load() ->
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:delete_all_objects(?TABLE_NAME),
    ets:from_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

save() ->
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:to_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

init() ->
    ets:new(?TABLE_NAME, [bag, public, named_table]).
