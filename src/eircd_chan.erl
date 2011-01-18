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


%% channel controller module CCM
%% One instance of this module is spawned foreach channel on server. Module keeps name of channel and list of users (tuples containing nickname and PID of middle man)
-module(eircd_chan).

-behaviour(gen_server).

-export([start_link/2]).

% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%inet: module
-include_lib("kernel/include/inet.hrl"). 

% data structure where current state of channel is saved (name and users)
-record(state, {name, users}).

start_link(Ref, ChanName) ->
	gen_server:start_link({global, Ref}, ?MODULE, [ChanName], []).

init([ChanName]) ->
	{ok, #state{name = ChanName, users=[]}}.

% returns list of users on channel
handle_call({who}, _From, State) ->
	{reply, State#state.users, State};

handle_call(Request, _From, State) ->
	io:fwrite("~p:handle_call: ~p~n", [?MODULE, Request]),
	{reply, ok, State}.

% send message to all users on channel
handle_cast({send, Action, Args}, State) ->
	io:format(standard_error, "~p:handle_cast({send, Action, Args}, State) is DEPRECATED~n", [?MODULE]),
	send(Action, Args, State),
	{noreply, State};

handle_cast({sendprefix, Prefix, Action, Args}, State) ->
	io:format(standard_error, "~p:handle_cast({sendprefix, Prefix, Action, Args}, State) is DEPRECATED~n", [?MODULE]),
	send(Prefix, Action, Args, State),
	{noreply, State};

% new form how sending should be handled
handle_cast({send, Sender, Action, Args}, State) ->
	case Action of
		"PRIVMSG" -> send_all_but(Action, Args, [Sender], State); % dont send PRIVMSG back to sender
		_ -> send(Action, Args, State)
	end,
	{noreply, State};

handle_cast({sendprefix, Sender, Prefix, Action, Args}, State) ->
	case Action of
		"PRIVMSG" -> send_all_but(Prefix, Action, Args, [Sender], State); % dont send PRIVMSG back to sender
		_ -> send(Prefix, Action, Args, State)
	end,
	{noreply, State};

% notification that user has left channel
handle_cast({part, Pid, Prefix, Reason}, State) ->
	case lists:keysearch(Pid, 1, State#state.users) of
		{value, UserEnt} -> send_all(State#state.users, Prefix, "PART", [State#state.name, Reason]),
							case State#state.users of
								[_OnlyUser] -> {stop, normal, State};
								_ -> {noreply, #state{name = State#state.name, users = lists:delete(UserEnt, State#state.users)}}
							end;
		false -> {noreply, State}
	end;

% notification that user has joined the channel, add him to list of users
handle_cast({join, UserPid, UserNick, UserName, UserHost}, State) -> 
	send(UserNick ++ "!" ++ UserName ++ "@" ++ UserHost, "JOIN", [State#state.name], State), % tell others :)
	{noreply, #state{name = State#state.name, users=[{UserPid, UserNick} | State#state.users]}};

% any other casts get cought here :)
handle_cast(Msg, State) -> 
	io:format(standard_error, "~p:hanle_cast ~p~n", [?MODULE, Msg]),
	{noreply, State}.

% everything get cought here
handle_info(Info, State) -> 
	io:fwrite("~p:handle_info: ~p~n", [?MODULE, Info]),
	{noreply, State}.

% hot swap code :)
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% what to do when we decide to die :)
terminate(Reason, State) -> 
	io:format(standard_error, "Chan controller ~p has quit! ~p~n", [State#state.name, Reason]),
	ok.

% sending messages to all users
send(Action, Args, State) ->
	send_all(State#state.users, Action, Args).

send(Prefix, Action, Args, State) ->
	send_all(State#state.users, Prefix, Action, Args).

send_all_but(Action, Args, Exceptions, State) ->
	send_all_but2(State#state.users, Action, Args, Exceptions).

send_all_but(Prefix, Action, Args, Exceptions, State) ->
	send_all_but2(State#state.users, Prefix, Action, Args, Exceptions).

% kinda nasty below here, janitor wanted!
% -------------------------
send_all([], _, _) -> ok;

send_all([{UserPid, _UserNick} | Rest], Action, Args) ->
	gen_server:cast(UserPid, {send, Action, Args}),
	send_all(Rest, Action, Args).

% -------------------------
send_all_but2([], _, _, _) -> ok;

send_all_but2([{UserPid, _UserNick} | Rest], Action, Args, Exceptions) ->
	case lists:member(UserPid, Exceptions)  of
		false -> gen_server:cast(UserPid, {send, Action, Args});
		true -> do_not_send
	end,
	send_all_but2(Rest, Action, Args, Exceptions).

% -------------------------
send_all([], _, _, _) -> ok;

send_all([{UserPid, _UserNick} | Rest], Prefix, Action, Args) ->
	gen_server:cast(UserPid, {sendprefix, Prefix, Action, Args}),
	send_all(Rest, Prefix, Action, Args).

% -------------------------
send_all_but2([], _, _, _, _) -> ok;

send_all_but2([{UserPid, _UserNick} | Rest], Prefix, Action, Args, Exceptions) ->
	case lists:member(UserPid, Exceptions) of
		false -> gen_server:cast(UserPid, {sendprefix, Prefix, Action, Args});
		true -> do_not_send
	end,
	send_all_but2(Rest, Prefix, Action, Args, Exceptions).