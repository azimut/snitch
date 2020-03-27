-module(snitch_client).

-export([get/1,all/0]).
-export([reload/0,checkpoint/0,add/1]).

reload()     -> gen_server:cast(snitch_scheduler, reload).
checkpoint() -> gen_server:cast(snitch_scheduler, checkpoint).
add(Domain)  -> gen_server:cast(snitch_scheduler, {add, Domain}).
all()        -> gen_server:call(snitch_scheduler, all).
get(Domain)  ->
    Ret = gen_server:call(snitch_scheduler, {get, Domain}),
    {Ret, calendar:gregorian_seconds_to_datetime(Ret)}.
