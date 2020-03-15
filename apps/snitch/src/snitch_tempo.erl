-module(snitch_tempo).
-export([get_random_time/0,has_time_expired/1]).

get_random_time() ->
    { rand:uniform(24), rand:uniform(60), rand:uniform(60) }.

has_time_expired({EH,EM,_}) ->
    {_, {NH, NM, _}} = calendar:local_time(),
    (NH > EH) and (NM > EM).
