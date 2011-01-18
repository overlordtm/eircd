%% Copyright (C) 2011 by Andraz Vrhovec <andraz@vrhovec.si>
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(eircd_mm).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/inet.hrl"). %inet: funkcije
-record(state, {nick, username, hostname, welcomed, socket, chans}).


start_link(Ref) ->
	gen_server:start_link({global, Ref}, ?MODULE, [], []).

init([]) ->
	{ok, #state{}}.

handle_call({who}, _From, State) ->
		{ok, Servername} = inet:gethostname(),
		{reply, {State#state.chans, State#state.username, State#state.hostname, Servername, State#state.nick}, State};

handle_call(Request, _From, State) ->
	io:fwrite("Call: ~p~n", [Request]),
	{reply, reply, State}.

handle_cast({create, Socket}, State) ->
	send(Socket, "NOTICE", ["AUTH", "*** Looking up your hostname..."]),
	{ok, {RAddr, _RPort}} = inet:peername(Socket),
	{ok, #hostent{h_name = Host}} = inet:gethostbyaddr(RAddr),
	send(Socket, "NOTICE", ["AUTH", "*** Found your hostname."]),
	{noreply, #state{nick = State#state.nick,
			username = State#state.username,
			hostname = Host,
			welcomed = false,
			socket = Socket,
			chans = []}};

handle_cast({send, Action, Args}, State) ->
    send(State#state.socket, Action, Args),
    {noreply, State};

handle_cast({sendprefix, Prefix, Action, Args}, State) ->
    send(State#state.socket, Prefix, Action, Args),
    {noreply, State};

handle_cast(Msg, State) -> 
	io:fwrite("Ne razumem casta ~p~n", [Msg]),
	{noreply, State}.

% dobimo tcp paket, razbijemo ga v seznam po vrsticah
handle_info({tcp, _Socket, Data}, State) -> 
	handle_irc(string:tokens(Data, "\r\n"), State);

% dobimo nekaj, kar ni take oblike kot hocemo
handle_info(Info, State) -> 
	io:fwrite("handle_info: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, _State) -> 
	io:fwrite("Uporabnik zapusca vas! ~p~n", [Reason]),
	ok.

handle_irc({_Prefix, Cmd, Args}, State) ->
	case Cmd of
		"NICK" -> 	nick(Args, State);
		"USER" -> 	user(Args, State);
		"JOIN" ->	join(Args, State);		
		"QUIT" ->	quit(Args, State);
		"PART" ->	part(Args, State);
		"PING" ->	{ok, Srv} = inet:gethostname(),
					gen_server:cast(self(), {send, "PONG", [Srv, hd(Args)]}),
					{noreply, State};
		"PRIVMSG" -> [Dest, Msg | _] = Args,
					 msg(State#state.nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, Dest, Cmd, Msg),
					 {noreply, State};
		"WHO" ->	who(Args, State);
		"WHOIS" ->	whois(Args, State);
		"MODE" -> mode(Args, State);
		_Anything -> send(State#state.socket, "421", [State#state.nick, "Unknown command " ++ Cmd]),
					 {noreply, State}
	end;

handle_irc([], State) ->
	State;

handle_irc([H | T], State) ->
	handle_irc(T, handle_irc(eirc:parse(H), State)).

send(Socket, Cmd, Args) ->
	{ok, Server} = inet:gethostname(),
	send(Socket, Server, Cmd, Args).

send(Socket, Prefix, Cmd, Args) ->
    Data = ":" ++ Prefix ++ " " ++ eirc:format(Cmd, Args),
    gen_tcp:send(Socket, Data).

send(Socket, Prefix, Cmd, Args, nocolon) ->
    Data = ":" ++ Prefix ++ " " ++ eirc:format(Cmd, Args, nocolon),
    gen_tcp:send(Socket, Data).

msg(User, Dest, Cmd, Msg) ->
    case gen_server:call({global, server},{get_chan, Dest}) of
	{ok, Pid} -> gen_server:cast(Pid, {sendprefix, self(), User, Cmd, [Dest, Msg]});
        fail -> case gen_server:call({global, server}, {get_user, Dest}) of
	            {ok, Pid} -> gen_server:cast(Pid, {sendprefix, User, Cmd, [Dest, Msg]});
	            fail -> io:fwrite("~s ne obstaja! ~n", [Dest])
	        end
    end.

welcome(Socket, Nick, User) ->
    {ok, Server} = inet:gethostname(),
    send(Socket, Server, "001", [Nick, "Welcome to the Internet Relay Network, " ++ User]),
    send(Socket, Server, "002", [Nick, "Your host is " ++ Server ++ ", running version NaN"]),
    send(Socket, Server, "003", [Nick, "This server wes created 01.01.2011"]),
	%send(Socket, Server, "004", [Nick, Server, "eircd", "iowghraAsORTVSxNCWqBzvdHtGp", "lvhopsmntikrRcaqOALQbSeIKVfMCuzNTGj"], nocolon),
	%send(Socket, Server, "005", [Nick, "UHNAMES SAFELIST HCN MAXCHANNELS=20 CHANLIMIT=#:20 MAXLIST=b:60,e:60,I:60 NICKLEN=30 CHANNELLEN=32 TOPICLEN=307 KICKLEN=307 AWAYLEN=307 MAXTARGETS=20", "are supported by this server"]),
	%send(Socket, Server, "005", [Nick, "WALLCHOPS WATCH=128 WATCHOPTS=A SILENCE=15 MODES=12 CHANTYPES=# PREFIX=(qaohv)~&@%+ CHANMODES=beI,kfL,lj,psmntirRcOAQKVCuzNSMTG NETWORK=SiOff CASEMAPPING=ascii EXTBAN=~,cqnr ELIST=MNUCT STATUSMSG=~&@%+", "are supported by this server"]),
	%send(Socket, Server, "005", [Nick, "EXCEPTS INVEX CMDS=KNOCK,MAP,DCCALLOW,USERIP", "are supported by this server"]),
    send(Socket, Server, "375", [Nick, "Message of the Day"]),
    send(Socket, Server, "372", [Nick, "Ce bo tole delalo sem car :)"]),
    send(Socket, Server, "376", [Nick, "End of /MOTD command"]).

% IRC commands handling

% NICK <nick>
% user have not provided nickname
nick([], State) ->
	send(State#state.socket, "431", ["No nickname given"]),
	{noreply, State};

nick([Nick | _], State) ->
	case gen_server:call({global, server}, {nick, self(), Nick}) of
		ok -> 	{User, Welcome} = case State#state.username of
									  undefined -> 	{undefined, false};
									  _SomeUser -> 	gen_server:cast({global, server}, {broadcast, State#state.nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, "NICK", [Nick]}),
													{Nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, true}
								  end,

				Welcomed = case {State#state.welcomed, Welcome} of
							   {false, true} -> irc_user:welcome(State#state.socket, Nick, User),
												true;
							   _Other -> State#state.welcomed
							end,
				{noreply, #state{nick=Nick, username=State#state.username, hostname=State#state.hostname, welcomed=Welcomed, socket=State#state.socket, chans=State#state.chans}};
		
		fail -> send(State#state.socket, "433", [Nick, "Nickname is already in use"]),
				{noreply, State}
	end.

% USER <user> <mode> <unused> <realname>
user([Username, _Mode, _Unused, _RealName], State) ->
	Nick = State#state.nick,
	Socket = State#state.socket,
	{_User, Welcome} = case Nick of
						   undefined -> {"!" ++ Username ++ "@" ++ State#state.hostname, false};
						   _ -> {Nick ++ "!" ++ Username ++ "@" ++ State#state.hostname, true}	
					   end,				
	Welcomed = case {State#state.welcomed, Welcome} of			   
				   {false, true} -> welcome(Socket, Nick, Nick ++ "!" ++ Username ++ "@" ++ State#state.hostname),				
									true;			
				   _ -> State#state.welcomed			   
			   end,
	{noreply, #state{nick=State#state.nick, username=Username, hostname=State#state.hostname, welcomed=Welcomed, socket=State#state.socket, chans=State#state.chans}};

user(_Anything, State) ->
	send(State#state.socket, "461", ["USER" "Not enough parameters"]),
	{noreply, State}.


% QUIT <reason>
quit([Reason | _], State) ->
	RealReason = case Reason of
				 [Reason2 | _] -> Reason2;
				 _ -> "Quitting ..."
			 end,
	quit_chans(State#state.chans, State#state.nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, RealReason),
	gen_server:cast({global, server}, {userquit, self()}),
	{stop, normal, State}.

quit_chans([], _Prefix, _Reason) -> ok;
quit_chans([{CPid, _CName} | Rest], Prefix, Reason) ->
	gen_server:cast(CPid, {part, self(), Prefix, Reason}),
	quit_chans(Rest, Prefix, Reason).

% PART <chan> <reason>
part([Chan, Reason | _], State) ->
	case lists:keyfind(Chan, 2, State#state.chans) of
		false -> ignore_him;
		{ChanPid, ChanName} -> case Reason of
								   [] -> RealReason = "Bye.";
								   _  -> RealReason = Reason
							   end,
							   gen_server:cast(ChanPid, {part, self(), State#state.nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, RealReason})
	end,
	{noreply, State}.

% JOIN <chan>

join([ChanName | _], State) ->
	case ChanName of
		[$# | _] -> case gen_server:call({global, server},{get_chan, ChanName}) of
						{ok, ChanPid} -> join2(ChanPid, ChanName, State), ok;
						fail -> case gen_server:call({global, server},{create_chan, ChanName}) of				
									{ok, ChanPid} -> join2(ChanPid, ChanName, State), ok;
									_ -> ChanPid = fail, fail			
								end				
					end;				
		_Otherwise -> ChanPid = fail, fail				
	end,
				
	case ChanPid of
		fail -> {noreply, State};			
		_Anything -> {noreply, #state{nick = State#state.nick, username = State#state.username,	hostname = State#state.hostname, welcomed = State#state.welcomed, socket = State#state.socket, chans = [{ChanPid, ChanName} | State#state.chans]}}				
	end.

join2(ChanPid, ChanName, State) ->
	gen_server:cast(ChanPid, {join, self(), State#state.nick, State#state.username, State#state.hostname}),
	send(State#state.socket, State#state.nick ++ "!" ++ State#state.username ++ "@" ++ State#state.hostname, "JOIN", [ChanName]),
	send(State#state.socket, "353", [State#state.nick ++ " = " ++ ChanName, list_users(ChanPid)]),
	send(State#state.socket, "366", [ChanName, "End of /NAMES list."]),
	%send(State#state.socket, "333", [State#state.nick, ChanName, io_lib:format("~p", [eirc:unix_seconds_since_epoch()])]),
	send(State#state.socket, "332", [ChanName, "DEFAULT TOPIC"]).

list_users(ChanPid) ->
	UserList = gen_server:call(ChanPid, {who}),
	eirc:list_to_string([element(2,User) || User <- UserList]). % list comprehension

% WHO [<chan> | <user>]
who([], State) -> {noreply, State};
who([First | Rest], State) ->
	who_single(First, State),
	who(Rest, State).

who_single(Query, State) ->
	case gen_server:call({global, server},{get_chan, Query}) of
	{ok, Pid} -> who_chan(Pid, State);
        fail -> case gen_server:call({global, server}, {get_user, Query}) of
	            {ok, Pid} -> who_user(Pid, State);
	            fail -> fail
	        end
    end,
	send(State#state.socket, "315", [State#state.nick, Query, "End of /WHO list."]).

who_user(Pid, State) ->
	Self = self(),
	case Pid of
		Self -> {ChanList, Username, Hostname, Servername, Nick} = {State#state.chans, State#state.username, State#state.hostname, element(2, inet:gethostname()), State#state.nick};
		_Anything -> {ChanList, Username, Hostname, Servername, Nick} = gen_server:call(Pid, {who})
	end,
	LastChan = case ChanList of
				   [] -> "*";
				   _Any -> hd([element(2,Chan) || Chan <- ChanList])

			   end,
	send(State#state.socket, "352", [State#state.nick, LastChan, Username, Hostname, Servername, Nick, "H", "0 Realname"]).

who_chan(Pid, State) ->
	UserList = gen_server:call(Pid, {who}),
	who_chan_loop(UserList, State).

who_chan_loop([], _State) -> ok;
who_chan_loop([{Pid, _Nick} | Rest], State) ->
	who_user(Pid, State),
	who_chan_loop(Rest, State).

% WHOIS
whois([User], State) ->
	Self = self(),
	case gen_server:call({global, server}, {get_user, User}) of
		{ok, Pid} -> case Pid of
						 Self -> {ok, Servername} = inet:gethostname(),
								 Record = {State#state.chans, State#state.username, State#state.hostname, Servername, State#state.nick},
								 whois2(Record, State);
						 _Anything -> Record =  gen_server:call(Pid, {who}),
									  whois2(Record, State)
					 end;
		fail -> fail
	end,
	{noreply, State}.

whois2({ChanList, Username, Hostname, Servername, Nick}, State) ->
	send(State#state.socket, "311", [State#state.nick, Nick, Username, Hostname, "*", "realname"]),
	io:format("Seznam kanalov ~p~n", [ChanList]),
	case ChanList of
		[] -> fail;
		_Else ->	send(State#state.socket, "319", [State#state.nick, Nick, eirc:list_to_string([element(2,Chan) || Chan <- ChanList])])
	end,
	send(State#state.socket, "312", [State#state.nick, Nick, Servername, "eircd"]),
	send(State#state.socket, "317", [State#state.nick, Nick, 42, "seconds idile (42 is answer to everything)"]),
	send(State#state.socket, "318", [State#state.nick, Nick, "End of /WHOIS list."]).

% MODE

% sprasuje po modu kanala
mode([[$# | _]], State) ->
	{noreply, State};

% nastavljamo mode za nick
mode([Nick, Mode], State) ->
	Nick = State#state.nick,
	send(State#state.socket, Nick, "MODE", [Mode]),
	{noreply, State};

mode(_Args, State) ->
	send(State#state.socket, "NOTICE", ["Do not understand this MODE command " ++ string:join(_Args, $ )]),
	{noreply, State}.