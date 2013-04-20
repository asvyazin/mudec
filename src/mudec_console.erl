-module(mudec_console).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, send/2, token_received/2, stop/1]).
-export([init/1, handle_cast/2, terminate/2]).
-export ([read_input_loop/1]).

-record(state, {connection}).

start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

send(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).

token_received(Pid, Token) ->
    gen_server:cast(Pid, {token, Token}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Address, Port]) ->
    {ok, ConnPid} = mudec_connection:start_link(Address, Port),
    mudec_connection:add_handler(ConnPid, mudec_console_handler, [self()]),
	spawn_link(?MODULE, read_input_loop, [ConnPid]),
    {ok, #state{connection = ConnPid}}.

handle_cast({send, Message}, #state{connection = Conn} = S) ->
    mudec_connection:send(Conn, [Message, "\r\n"]),
    {noreply, S};
handle_cast ({token, {command, 249}}, #state{} = S) ->
	{noreply, S};
handle_cast ({token, {will, 1}}, #state{} = S) ->
	{noreply, S};
handle_cast ({token, {will, Option}}, #state{} = S) ->
	io:format("Unknown WILL: ~p~n", [Option]),
	{noreply, S};
handle_cast ({token, {wont, 1}}, #state{} = S) ->
	{noreply, S};
handle_cast({token, {wont, Option}}, #state{} = S) ->
	io:format("Unknown WONT: ~p~n", [Option]),
	{noreply, S};
handle_cast ({token, {do, Option}}, #state{} = S) ->
	io:format("Unknown DO: ~p~n", [Option]),
	{noreply, S};
handle_cast ({token, {dont, Option}}, #state{} = S) ->
	io:format("Unknown DONT: ~p~n", [Option]),
	{noreply, S};
handle_cast ({token, {subnego, Option, Data}}, #state{} = S) ->
	io:format("Unknown SUBNEGO: ~p, ~p~n", [Option, Data]),
	{noreply, S};
handle_cast({token, Token}, #state{} = S) ->
    io:format("~s~n", [Token]),
    {noreply, S};
handle_cast(stop, #state{} = S) ->
    {stop, normal, S}.

terminate(normal, #state{}) ->
    ok.

read_input_loop(Pid) ->
	Input = io:get_line("> "),
	mudec_connection:send(Pid, Input),
	?MODULE:read_input_loop(Pid).