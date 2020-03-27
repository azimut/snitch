-module(snitch_disk).
-include("db.hrl").
-export(
   [
    save/0,
    load/0
   ]).

%% https://github.com/bernardd/Conway
%% /blob/d1af91a0b535978995c9644a9a91a2e658e478a6/cell_store.erl
load() ->
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:delete_all_objects(?TABLE_NAME),
    ets:from_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

save() -> 
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:to_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

