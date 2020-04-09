-module(snitch_alert).
-export([alert_on_difference/4]).

%% API

alert_on_difference(_,_,_Dns=[_|_],_Ets=[])        -> ok; % first time
alert_on_difference(_,_,Idem,Idem)                 -> ok; % equal
alert_on_difference(_,_,["timeout"],_)             -> ok; % do not alert on timeout
alert_on_difference(Domain,a,Dns,Ets)              ->
    Diff = snitch_locus:new_asns(Dns,Ets),
    alert(Domain, a, Dns, Ets, Diff);
alert_on_difference(Domain,aaaa,Dns,Ets)           ->
    Diff = snitch_locus:new_asns(Dns,Ets),
    alert(Domain, a, Dns, Ets, Diff);
alert_on_difference(Domain, cname, RawDns, RawEts) ->     % check ips only
    Dns = lists:filter(fun helpers:is_ip/1, RawDns),
    Ets = lists:filter(fun helpers:is_ip/1, RawEts),
    Diff = snitch_locus:new_asns(Dns,Ets),
    alert(Domain, cname, Dns, Ets, Diff);
alert_on_difference(Domain, Type, Dns, Ets)        ->
    Diff = helpers:subtract(Dns,Ets),
    alert(Domain, Type, Dns, Ets, Diff).

%% Internal functions

notify(Title, Msg) ->
    S = io_lib:format("/usr/bin/notify-send --urgency critical '~s' '~s'", [Title, Msg]),
    os:cmd(S).

alert_change(Domain, Type, Dns, Ets) ->
    logger:error("CHANGE! data for domain ~s", [Domain]),
    Msg = io_lib:format("type (~s) changed", [Type]),
    helpers:format_string(Msg),
    notify(Domain, Msg),
    helpers:format_list("OLD ~s~n", Ets),
    helpers:format_list("NEW ~s~n", Dns).
alert_new(Domain, Type, H) ->
    logger:error("NEW! data for domain ~s", [Domain]),
    Msg = io_lib:format("type ~s has new data:~n~s",[Type, H]),
    helpers:format_string(Msg),
    notify(Domain, Msg).

alert(_,_,_,_,[])                   -> ok; % DNS =:= ETS
alert(Domain, Type, Dns, Ets, Diff) ->
    DateTime = snitch_store:lookup_datetime(Domain, Type),
    Life = snitch_tempo:datetime_older_than_seconds(DateTime,2*24*60*60),
    alert(Domain,Type,Dns,Ets,Diff,Life).

alert(_,_,_,_,_,hot)                 -> ok;
alert(Domain,Type,[H|_],_,_,cold)    -> alert_new(Domain, Type, H);
alert(Domain,Type,Dns,Ets,Diff,cold) ->
    logger:error("HEY - Difference on ~p",[Diff]),
    alert_change(Domain, Type, Dns, Ets).
