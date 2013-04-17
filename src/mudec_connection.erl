-module(mudec_connection).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, add_handler/3, send/2]).
-export([init/1, handle_cast/2]).
-export([read_loop/2]).

-record(state, {event_mgr, sock}).

start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

add_handler(Pid, Handler, Args) ->
    ok = gen_server:cast(Pid, {add_handler, Handler, Args}).

send(Pid, Packet) ->
    ok = gen_server:cast(Pid, {send, Packet}).

init([Address, Port]) ->
    {ok, EventMgrRef} = gen_event:start_link(),
    {ok, Sock} = start_read_loop(Address, Port, EventMgrRef),
    {ok, #state{event_mgr = EventMgrRef, sock = Sock}}.

handle_cast({add_handler, Handler, Args}, #state{event_mgr = EventMgrRef} = S) ->
    ok = gen_event:add_handler(EventMgrRef, Handler, Args),
    {noreply, S};
handle_cast({send, Packet}, #state{sock = Sock} = S) ->
    ok = gen_tcp:send(Sock, Packet),
    {noreply, S}.

start_read_loop(Address, Port, EventMgrRef) ->
    {ok, Sock} = gen_tcp:connect(Address, Port, [{active, false}, {mode, list}]),
    {ok, TokenizerPid} = telnet_tokenizer:start_link(Sock, gen_tcp),
    ok = gen_tcp:controlling_process(Sock, TokenizerPid),
    spawn_link(?MODULE, read_loop, [TokenizerPid, EventMgrRef]),
    {ok, Sock}.

read_loop(TokenizerPid, EventMgrRef) ->
    {ok, Tokens} = telnet_parser:parse_and_scan({telnet_tokenizer, token, [TokenizerPid]}),
    [gen_event:notify(EventMgrRef, Token) || Token <- Tokens],
    ?MODULE:read_loop(TokenizerPid, EventMgrRef).
