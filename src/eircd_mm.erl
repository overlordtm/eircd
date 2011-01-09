-module(eircd_mm).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_irc/2, send/3, send/4, welcome/3, quit/4]).

-include_lib("kernel/include/inet.hrl"). %inet: funkcije
-record(state, {nick, user, host, welcomed, socket, chans}).


start_link(Ref) ->
	gen_server:start_link({global, Ref}, ?MODULE, [], []).

init([]) ->
	io:fwrite("nov uporabnik je prisel! ~n"),
	{ok, #state{}}.

handle_call(Request, _From, State) ->
	io:fwrite("Call: ~p~n", [Request]),
	{reply, reply, State}.

handle_cast({create, Socket}, State) ->
	{ok, {RAddr, _RPort}} = inet:peername(Socket),
	{ok, #hostent{h_name = Host}} = inet:gethostbyaddr(RAddr),
	{noreply, #state{nick = State#state.nick,
			user = State#state.user,
			host = Host,
			welcomed = false,
			socket = Socket,
			chans = []}};

handle_cast(quit, State) ->
	io:fwrite("QUITTING ..."),
	{stop, normal, State};

handle_cast(Msg, State) -> 
	io:fwrite("Ne razumem casta ~p~n", [Msg]),
	{noreply, State}.

handle_info({tcp, _Socket, Data}, State) -> 
	%io:fwrite("Sporocilo je: ~p~n", [Data]),
	eircd_mm:handle_irc(eirc:parse(Data), State),
	{noreply, State};

handle_info(Info, State) -> 
	io:fwrite("tcp msg: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) -> 
	io:fwrite("Uporabnik zapusca vas! ~p~n", [Reason]),
	ok.

handle_irc({Prefix, Cmd, Args}, State) ->
	io:fwrite("Prefix: ~p, Cmd: ~p, Args, ~p~n", [Prefix, Cmd, Args]),
	case Cmd of
		"NICK" -> 	Nick = hd(Args),
				case gen_server:call({global, server}, {nick, self(), Nick}) of
					ok -> 	{User, Welcome} = case State#state.user of
							undefined -> 	{undefined, false}; %torej imamo novega uporabnika, ki se nima usernejma
							SomeUser -> 	gen_server:cast({global, server}, {broadcast, SomeUser, "NICK", Nick}),
							{Nick ++ "!" ++ "nekikarjetrebazracunat" ++ "@" ++ State#state.host, true}
						end,
						Welcomed = case {State#state.welcomed, Welcome} of
							{false, true} -> eircd_mm:welcome(State#state.socket, Nick, User), true;
							_Otherwise -> State#state.welcomed
						end,
						io:fwrite("Nick je natsvljen na ~s~n", [Nick]),
						{noreply, #state{
							nick=Nick,
							user=State#state.user,
							host=State#state.host,
							welcomed=State#state.welcomed,
							socket=State#state.socket,
							chans=State#state.chans
							}
						};
					fail -> eircd_mm:send(State#state.socket, "NOTICE", [Nick, " is occupied! Use another one!"])
				end;
		"USER" -> 	[Nick, Username, Hostname, _Realname] = Args,
					Socket = State#state.socket,
					eircd_mm:welcome(Socket, Nick, Username),
					{noreply, #state{nick=State#state.nick,
						user=Username,
						host=Hostname,
						welcomed=true,
						socket=State#state.socket,
						chans=State#state.chans}
					};
		"JOIN" ->	gen_server:cast({global, server}, {chan, join, self(), hd(Args)}),
					{noreply, state};
		"QUIT" ->	Reason = case Args of
								[Reason2 | _] -> Reason2;
								_ -> "no reason"
							 end,
					eircd_mm:quit(self(), State#state.user, State#state.chans, Reason);
		"PING" ->	Socket = State#state.socket,
					{ok, Srv} = inet:gethostname(),
					eircd_mm:send(Socket, "PONG", [Srv, Srv]),
					{noreply, State};	
		"MODE" ->	io:fwrite("Prejel MODE"),
					Socket = State#state.socket,
					Nick = State#state.nick,
					eircd_mm:send(Socket, "MODE", [Nick, "+i"]);
		_Anything -> io:fwrite("nepohendlan ukaz"), {noreply, State}
	end;

handle_irc([], State) ->
	State;

handle_irc([H | T], State) ->
	eircd_mm:handle_irc(T, eircd_mm:handle_irc(H, State)).

send(Socket, Cmd, Args) ->
    {ok, Server} = inet:gethostname(),
    eircd_mm:send(Socket, Server, Cmd, Args).

send(Socket, Servername, Cmd, Args) ->
    Data = ":" ++ Servername ++ " " ++ eirc:format(Cmd, Args),
    io:fwrite("sending on tcp: ~p~n", [Data]),
    gen_tcp:send(Socket, Data).

welcome(Socket, Nick, User) ->
    {ok, Server} = inet:gethostname(),
    eircd_mm:send(Socket, "001", [Nick, "Welcome to the Internet Relay Network, "++User]),
    eircd_mm:send(Socket, "002", [Nick, "your host is "++Server++", running version NaN"]),
    eircd_mm:send(Socket, "003", [Nick, "this server wes created yesterday"]),
    eircd_mm:send(Socket, "375", [Nick, "- "++Server++" Message of the Day -"]),
    eircd_mm:send(Socket, "372", [Nick, "Ce bo tole delalo sem car :)"]),
    eircd_mm:send(Socket,"376", [Nick, "End of /MOTD command"]),
    eircd_mm:send(Socket,"NOTICE", [Nick, "this is a little server and only few commands are implemented"]).

quit(Pid, User, Chans, Reason) ->
	io:format("Zapustit moram vse kanale!~n"),
	io:fwrite("POvedat moram serverju da sem sel~n"),
	io:fwrite("Zapuscam zaradi ~p~n", [Reason]),
	gen_server:cast(Pid, quit).
	
