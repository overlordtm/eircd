-module(eircd).
-compile(export_all).

server() ->
	{ok, Listen} = gen_tcp:listen(6667, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
	spawn(fun() -> conn_worker(Listen) end).

conn_worker(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> conn_worker(Listen) end),
	loop(Socket).

loop(Socket) ->
	receive
		{tcp, Socket, Data} ->
			process_messages(Socket, string:tokens(binary_to_list(Data), "\r\n")),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Server socket closed")
	end.

chan(Socket, Clients) ->
	receive
		{tcp, Socket, Data} -> chan(Socket, Clients);
		{tcp_closed, Socket} -> io:format("Povezava prekinjena")
	end.

process_messages(Socket, []) -> true;
process_messages(Socket, [H | T]) ->
	case parse(H) of
		{_Prefix, "NICK", _Args} -> true;
		{_Prefix, "USER", _Args} -> gen_tcp:send(Socket, list_to_binary(":localhost 001 az :Welcome!\r\n:localhost 002 az :Your host ist localhost\r\n:localhost 003 :Created on lalalaa\r\n:localhost 005 :Serverinfo"));
		{_Prefix, "MODE", _Args} -> gen_tcp:send(Socket, list_to_binary(":az MODE az :+i"));
		{_Prefix, "PING", _Args} -> gen_tcp:send(Socket, list_to_binary(":localhost PONG localhost :localhost"));
		{_Prefix, "QUIT", _Args} -> true;
		{_Prefix, "JOIN", Args} -> [Chan|T] = Args, 
			case whereis(Chan) of
				Pid when is_number(Pid) -> Pid ! {chan, newClient, self()};
				undefined -> Pid = spawn(fun() -> chan(Socket, self())), register(Chan, Pid)
			end;
		{_Prefix, "PRIVMSG", Args} -> gen_tcp:send(Socket, list_to_binary(":nick!user@host PRIVMSG #kanal :nekoSporocilo"));
		{_Prefix, Other, Args} -> gen_tcp:send(Socket, list_to_binary(":nick!user@host PRIVMSG #kanal :neko sporocilo!"))
	end,
	process_messages(Socket, T).

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
	{none, Command, params(Params)}.

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
