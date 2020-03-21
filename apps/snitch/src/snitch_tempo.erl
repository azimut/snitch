-module(snitch_tempo).
-export(
   [
    get_future_gregorian/0,
    has_gregorian_passed/1
   ]).

get_future_gregorian() ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Offset = rand:uniform(60 * 60 * 24),
    Now + Offset.

has_gregorian_passed(Gregorian) ->
    NowGregorian = calendar:datetime_to_gregorian_seconds(
                     calendar:local_time()),
    NowGregorian > Gregorian.