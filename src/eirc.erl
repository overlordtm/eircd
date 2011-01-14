-module(eirc).

-export([parse/1, format/2, format/3, unix_seconds_since_epoch/0, list_to_string/1, list_to_string/2]).

parse(Msg) ->
	parse2(strip_crlf(Msg)).

parse2([$: | Msg]) ->
	Tokens = string:tokens(Msg, " "),
	[Prefix | Rest] = Tokens,
	[Command | Params] = Rest,
	{Prefix, Command, params(Params)};
parse2(Msg) -> 
	Tokens = string:tokens(Msg, " "),
	[Command | Params] = Tokens,
	{undefined, Command, params(Params)}.

params(L) ->
	params(L, []).

params([], P) ->
	lists:reverse(P);
params([[$: | HT] | T], P) ->
	params([], [string:join([HT | T], " ") | P]);
params([H | T], P) ->
	params(T, [H | P]).
	
strip_crlf(Str) ->
	string:strip(string:strip(Str, right, $\n), right, $\r).

format(Command, []) ->
	[Command, "\r\n"];
format(Command, [Arg]) ->
	[Command, " :", Arg, "\r\n"];
format(Command, Args) ->
    [Last | Rev] = lists:reverse(Args),
    [Command, $ , join(lists:reverse(Rev), $ ), " :", Last, "\r\n"].

format(Command, [], nocolon) ->
	[Command, "\r\n"];
format(Command, [Arg], nocolon) ->
	[Command, Arg, "\r\n"];
format(Command, Args, nocolon) ->
    [Command, $ , join(Args, $ ), "\r\n"].

join([], _) -> [];
join([A], _) -> [A];
join([H | T], Sep) -> [H, Sep, join(T, Sep)].

unix_seconds_since_epoch() ->
    LocalDateTime = calendar:datetime_to_gregorian_seconds({date(),time()}),
    UnixEpoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    LocalDateTime - UnixEpoch.

list_to_string([H | T]) -> list_to_string2(T, H, " ").
list_to_string([H | T], Separator) -> list_to_string2(T, H, Separator).

list_to_string2([], String, _) -> String;
list_to_string2([H | T], String, Separator) ->
	list_to_string2(T, String ++ Separator ++ H, Separator).
	
