-module(snitch_client).
-export(
   [
    get/1,
    add/1,
    all/0
   ]).

all() ->
    gen_server:call(snitch_scheduler, all).
get(Domain) ->
    Ret = gen_server:call(snitch_scheduler, {get, Domain}),
    {Ret, calendar:gregorian_seconds_to_datetime(Ret)}.
add(Domain) ->
    gen_server:cast(snitch_scheduler, {add, Domain}).
