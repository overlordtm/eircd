-module(eircd_mm).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([handle_irc/2, send/3, send/4, welcome/3, quit/4, msg/4]).

-include_lib("kernel/include/inet.hrl"). %inet: funkcije
-record(state, {nick, user, host, welcomed, socket, chans}).


start_link(Ref) ->
	gen_server:start_link({global, Ref}, ?MODULE, [], []).

init([]) ->
	io:fwrite("nov uporabnik je prisel! ~n"),
	{ok, #state{}}.

handle_call(userinfo, _From, State) ->
		Nick = State#state.nick,
		User = State#state.user,
		Host = State#state.host,
		{reply, {Nick, User, Host}, State};

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

handle_cast({send, Action, Args}, State) ->
    eircd_mm:send(State#state.socket, Action, Args),
    {noreply, State};

handle_cast({sendprefix, Prefix, Action, Args}, State) ->
    eircd_mm:send(State#state.socket, Prefix, Action, Args),
    {noreply, State};

handle_cast({join, Pid}, State) ->
    {noreply, #state{
					nick = State#state.nick,
					user = State#state.user,
                    host = State#state.host,
		     		welcomed = State#state.welcomed,
					socket = State#state.socket,
					chans = [Pid | State#state.chans]}
	};

handle_cast(quit, State) ->
	io:fwrite("QUITTING ..."),
	{stop, normal, State};

handle_cast(Msg, State) -> 
	io:fwrite("Ne razumem casta ~p~n", [Msg]),
	{noreply, State}.

% dobimo tcp paket, razbijemo ga v seznam po vrsticah
handle_info({tcp, _Socket, Data}, State) -> 
	eircd_mm:handle_irc(string:tokens(Data, "\r\n"), State);

% dobimo nekaj, kar ni take oblike kot hocemo
handle_info(Info, State) -> 
	io:fwrite("handle_info: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) -> 
	io:fwrite("Uporabnik zapusca vas! ~p~n", [Reason]),
	ok.

handle_irc({Prefix, Cmd, Args}, State) ->
	io:fwrite("KOMANDA: Prefix: ~p, Cmd: ~p, Args, ~p~n", [Prefix, Cmd, Args]),
	io:fwrite("STANJE: ~p~n", [State]),
	case Cmd of
		"NICK" -> 	Nick = hd(Args),
				case gen_server:call({global, server}, {nick, self(), Nick}) of
					ok -> 	{User, Welcome} = case State#state.user of
								undefined -> 	{undefined, false}; %torej imamo novega uporabnika, ki se nima usernejma
								SomeUser -> 	gen_server:cast({global, server}, {broadcast, SomeUser, "NICK", Nick}),
								{Nick ++ "!" ++ "nekikarjetrebazracunat" ++ "@" ++ State#state.host, true}
							end,
							Welcomed = case {State#state.welcomed, Welcome} of
		      	                   {false, true} -> irc_user:welcome(State#state.socket, hd(Args), User),
													true;
		                           _ -> State#state.welcomed
							end,
							
						{noreply, #state{
							nick=Nick,
							user=State#state.user,
							host=State#state.host,
							welcomed=Welcomed,
							socket=State#state.socket,
							chans=State#state.chans
							}
						};
					fail -> eircd_mm:send(State#state.socket, "NOTICE", [Nick, " is occupied! Use another one!"]),
							{noreply, State}
				end;
		"USER" -> 	[Username, Hostname, _Servername, _Realname] = Args,
					Nick = State#state.nick,
					gen_server:cast(self(), {send, "020", [Nick, "*", "Please wait while we process your connection."]}),
					Socket = State#state.socket,
					{User, Welcome} = case Nick of
										  undefined -> {"!"++hd(Args)++"@"++State#state.host, false};
										  _ -> {Nick ++ "!" ++ Username ++ "@" ++ Hostname, true}
									  end,
					Welcomed = case {State#state.welcomed, Welcome} of
								   {false, true} -> eircd_mm:welcome(Socket, Nick, User),
													true;
									_ -> State#state.welcomed
							   end,
					{noreply, #state{nick=State#state.nick,
						user=Username,
						host=Hostname,
						welcomed=Welcomed,
						socket=State#state.socket,
						chans=State#state.chans}
					};
		"JOIN" ->	gen_server:cast({global, server}, {chan, join, self(), hd(Args)}),
					{noreply, state};
		"QUIT" ->	Reason = case Args of
								[Reason2 | _] -> Reason2;
								_ -> "no reason"
							 end,
					eircd_mm:quit(self(), State#state.user, State#state.chans, Reason),
					{noreply, State};
		"PING" ->	{ok, Srv} = inet:gethostname(),
					gen_server:cast(self(), {send, "PONG", [Srv, hd(Args)]}),
					{noreply, State};
		"PRIVMSG" -> [Dest, Msg | _] = Args,
					 eircd_mm:msg(State#state.user, Dest, Cmd, Msg),
					 {noreply, State};
		_Anything -> eircd_mm:send(State#state.socket, "421", [State#state.nick, "Unknown command"]),
					 {noreply, State}
	end;

handle_irc([], State) ->
	State;

handle_irc([H | T], State) ->
	eircd_mm:handle_irc(T, eircd_mm:handle_irc(eirc:parse(H), State)).

send(Socket, Cmd, Args) ->
	{ok, Server} = inet:gethostname(),
	eircd_mm:send(Socket, Server, Cmd, Args).

send(Socket, Prefix, Cmd, Args) ->
    Data = ":" ++ Prefix ++ " " ++ eirc:format(Cmd, Args),
    %io:fwrite("sending on tcp: ~p~n", [Data]),
    gen_tcp:send(Socket, Data).

msg(User, Dest, Cmd, Msg) ->
    case gen_server:call({global, server},{get_chan, Dest}) of
	{ok, Pid} -> gen_server:cast(Pid,{msg, User, Cmd, Msg});
        fail -> case gen_server:call({global, server},{get_user, Dest}) of
	            {ok, Pid} -> gen_server:cast(Pid,{sendprefix, User, Cmd, [Msg]});
	            fail -> io:fwrite("~s ne obstaja! ~n", [Dest])
	        end
    end.

welcome(Socket, Nick, User) ->
    {ok, Server} = inet:gethostname(),
    eircd_mm:send(Socket, Server, "001", [Nick, "Welcome to the Internet Relay Network, " ++ User]),
    eircd_mm:send(Socket, Server, "002", [Nick, "Your host is " ++ Server ++ ", running version NaN"]),
    eircd_mm:send(Socket, Server, "003", [Nick, "This server wes created yesterday :p"]),
    eircd_mm:send(Socket, Server, "375", [Nick, "Message of the Day"]),
    eircd_mm:send(Socket, Server, "372", [Nick, "Ce bo tole delalo sem car :)"]),
    eircd_mm:send(Socket, Server, "376", [Nick, "End of /MOTD command"]).

quit(Pid, _User, _Chans, Reason) ->
	io:format("Zapustit moram vse kanale!~n"),
	io:fwrite("POvedat moram serverju da sem sel~n"),
	io:fwrite("Zapuscam zaradi ~p~n", [Reason]),
	gen_server:cast(Pid, quit).
	
