-module(mudec_console).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, send/2, token_received/2]).
-export([init/1, handle_cast/2]).

-record(state, {connection}).

start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

send(Pid, Message) ->
    gen_server:cast(Pid, {send, Message}).

token_received(Pid, Token) ->
    gen_server:cast(Pid, {token, Token}).

init([Address, Port]) ->
    {ok, ConnPid} = mudec_connection:start_link(Address, Port),
    mudec_connection:add_handler(ConnPid, mudec_connection_handler, [self()]),
    {ok, #state{connection = ConnPid}}.

handle_cast({send, Message}, #state{connection = Conn} = S) ->
    mudec_connection:send(Conn, Message),
    {noreply, S};
handle_cast({token, Token}, #state{} = S) ->
    io:format("~p~n", [Token]),
    {noreply, S}.
