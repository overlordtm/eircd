%#!/usr/bin/env escript

-module(eircd).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_connection/1]).

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
		{value, {_WrongPid, Nick}} -> {reply, fail, State}; %uporabnik s tem nickom ze obstaja
		false -> case lists:keysearch(Pid, 1, State#state.users) of
			{value, {Pid, OldNick}} -> {reply, ok, #state{users=[{Pid, Nick} | lists:delete({Pid, OldNick}, State#state.users)], chans=State#state.chans}};
			false -> {reply, ok, #state{users=[{Pid, Nick} | State#state.users], chans=State#state.chans}}
		end
	end;

handle_call(Request, _From, State) ->
	io:fwrite("Call: ~p~n", [Request]),
	{reply, reply, State}.

handle_cast(Msg, State) -> 
	io:fwrite("Cast: ~p~n", [Msg]),
	{noreply, State}.

handle_info(Info, State) -> 
	io:fwrite("tcp msg: ~p~n", [Info]),
	{noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

accept_connection(LSocket) ->
	io:fwrite("Poslusam za povezave ~n"),
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			io:fwrite("Nekod se je povezal! ~n"),
			Ref = erlang:make_ref(),
			{ok, Pid} = eircd_mm:start_link(Ref),
			gen_tcp:controlling_process(Socket, Pid),
			gen_server:cast(Pid, {create, Socket}),
			eircd:accept_connection(LSocket);
		{error, Error} -> 
			io:fwrite("Napaka pri accept_connection ~p~n", [Error]),
			ircd:accept_connection(LSocket)
	end.
	
