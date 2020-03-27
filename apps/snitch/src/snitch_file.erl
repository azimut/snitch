-module(snitch_file).
-export([load/0]).
-include("db.hrl").

load() ->
    Domains = get_domains(?DNS_FILE),
    domains_to_dict(Domains).

get_domains(File) ->
    {ok, Binary} = file:read_file(File),
    Domains = string:tokens(
                erlang:binary_to_list(Binary), "\n\r\t"),
    lists:map(fun string:lowercase/1, Domains).

domains_to_dict(Domains) ->
    L = [{D, snitch_tempo:get_future_gregorian()} || D <- Domains],
    dict:from_list(L).
