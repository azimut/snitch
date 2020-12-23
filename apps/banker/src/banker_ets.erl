-module(banker_ets).
-define(TABLE_NAME, centralbank).

-export([load/0,save/0,init/0,insert/3]).

init() ->
    ets:new(?TABLE_NAME, [bag, public, named_table]).

load() ->
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:delete_all_objects(?TABLE_NAME),
    ets:from_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

save() ->
    {ok, Dets} = dets:open_file(?TABLE_NAME, [{type,bag}]),
    ets:to_dets(?TABLE_NAME, Dets),
    dets:close(Dets).

insert(Domain, Type, Data) ->
    ets:insert_new(?TABLE_NAME, {Domain, Type, Data}).

%% now_gregorian_seconds() ->
%%     calendar:datetime_to_gregorian_seconds(
%%       calendar:local_time()).
