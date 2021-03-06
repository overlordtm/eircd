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

-module(eircd).
-behaviour(gen_server). 

-export([start_link/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_connection/1, broadcast/4]).

% nekj takega ko struct v C
-record(state, {users, chans}).

% gen_server:start_link(ServerName, Module, Args, Options) -> Result
start_link() ->
	gen_server:start_link({global, server}, ?MODULE, [], []).

init([]) ->
	io:fwrite("Zaganjam, v init fazi ~n"),
	case gen_tcp:listen(6667, [{packet, line}, {reuseaddr, true}]) of
		{ok, LSocket} -> spawn(eircd, accept_connection, [LSocket]);
		{error, Error} -> io:fwrite("Ne morem poslusat na 6667: ~p~n", [Error])
	end,
	{ok, #state{users=[], chans=[]}}.

handle_call({nick, Pid, Nick}, _From, State) ->
	case lists:keysearch(Nick, 2, State#state.users) of
		{value, {_WrongPid, Nick}} -> 
			{reply, fail, State}; %uporabnik s tem nickom ze obstaja
		false -> 
			case lists:keysearch(Pid, 1, State#state.users) of
				{value, {Pid, OldNick}} -> 
					{reply, ok, #state{users=[{Pid, Nick} | lists:delete({Pid, OldNick}, State#state.users)], chans=State#state.chans}};
				false ->
					{reply, ok, #state{users=[{Pid, Nick} | State#state.users], chans=State#state.chans}}
			end
	end;

handle_call({get_chan, Chan}, _From, State) ->
	case lists:keysearch(Chan, 2, State#state.chans) of
		{value, {Pid, _Chan}} -> {reply, {ok, Pid}, State};
		false -> {reply, fail, State}
    end;

handle_call({get_user, Nick}, _From, State) ->
	case lists:keysearch(Nick, 2, State#state.users) of
		{value, {Pid, Nick}} -> {reply, {ok, Pid}, State};
		false -> {reply, fail, State}
    end;

handle_call({create_chan, Chan}, _From, State) ->
	Ref = erlang:make_ref(),
	{ok, Pid} = eircd_chan:start_link(Ref, Chan),
	{reply, {ok, Pid}, #state{users=State#state.users, chans=[{Pid, Chan} | State#state.chans]}};

handle_call(Msg, From, State) ->
	io:fwrite("Call from ~p: ~p~n", [From, Msg]),
	{noreply, State}.

handle_cast({broadcast, Prefix, Cmd, Args}, State) ->
   	broadcast(State#state.users, Prefix, Cmd, Args),
    {noreply, State};

handle_cast({userquit, Pid}, State) ->
	case lists:keysearch(Pid, 1, State#state.users) of
		{value, UserEnt} -> {noreply, #state{users = lists:delete(UserEnt, State#state.users), chans = State#state.chans}};
		false -> {noreply, State}
	end;

handle_cast(Msg, State) -> 
	io:fwrite("Cast: ~p~n", [Msg]),
	{noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    io:format("tcp message: ~p~n", [Data]),
    server:handle_irc(Socket,eirc:parse(Data)),
    {noreply, State};

handle_info(Info, State) -> 
	io:fwrite("tcp msg: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

accept_connection(LSocket) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			Ref = erlang:make_ref(),
			{ok, Pid} = eircd_mm:start_link(Ref),
			gen_tcp:controlling_process(Socket, Pid),
			gen_server:cast(Pid, {create, Socket}),
			eircd:accept_connection(LSocket);
		{error, Error} -> 
			io:fwrite("Napaka pri accept_connection ~p~n", [Error]),
			ircd:accept_connection(LSocket)
	end.
	
broadcast([], _Prefix, _Action, _Args) -> ok;

broadcast([{Pid, _} | Rest], Prefix, Cmd, Args) ->
	gen_server:cast(Pid, {sendprefix, Prefix,  Cmd, Args}),
	eircd:broadcast(Rest, Prefix, Cmd, Args).