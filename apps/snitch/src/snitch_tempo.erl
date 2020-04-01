-module(snitch_tempo).
-include("db.hrl").
-export(
   [
    get_future_gregorian/0,
    has_gregorian_passed/1,
    datetime_older_than_seconds/2
   ]).

get_future_gregorian() ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Offset = rand:uniform(?RANGE_HOURS * 60 * 60),
    Now + Offset.

has_gregorian_passed(Gregorian) ->
    NowGregorian = calendar:datetime_to_gregorian_seconds(
                     calendar:local_time()),
    NowGregorian >= Gregorian.

datetime_older_than_seconds(Datetime, Seconds) ->
    Now = calendar:datetime_to_gregorian_seconds(
            calendar:local_time()),
    Then = calendar:datetime_to_gregorian_seconds(Datetime),
    Diff = Now - Then,
    case Diff > Seconds of
        true  -> cold;
        false -> hot
    end.
