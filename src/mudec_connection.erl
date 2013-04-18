-module(mudec_connection).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, add_handler/3, send/2]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {event_mgr, sock, cont}).

-define(SOCK_OPTIONS, [{active, once}, {mode, list}]).

start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

add_handler(Pid, Handler, Args) ->
    ok = gen_server:cast(Pid, {add_handler, Handler, Args}).

send(Pid, Packet) ->
    ok = gen_server:cast(Pid, {send, Packet}).

init([Address, Port]) ->
    {ok, EventMgrRef} = gen_event:start_link(),
    {ok, Sock} = gen_tcp:connect(Address, Port, ?SOCK_OPTIONS),
    {ok, #state{event_mgr = EventMgrRef, sock = Sock, cont = []}}.

handle_cast({add_handler, Handler, Args}, #state{event_mgr = EventMgrRef} = S) ->
    ok = gen_event:add_handler(EventMgrRef, Handler, Args),
    {noreply, S};
handle_cast({send, Packet}, #state{sock = Sock} = S) ->
    ok = gen_tcp:send(Sock, Packet),
    {noreply, S}.

handle_info({tcp, _Sock, Data}, #state{sock = Sock, cont = Cont, event_mgr = EventMgrRef} = S) ->
    {ok, Tokens, NewCont, Line} = get_tokens(Data, Cont),
    {ok, ParsedTokens} = telnet_parser:parse(Tokens ++ [{'$end', Line}]),
    [gen_event:notify(EventMgrRef, {token, Token}) || Token <- ParsedTokens],
    inet:setopts(Sock, ?SOCK_OPTIONS),
    {noreply, S#state{cont = NewCont}};
handle_info({tcp_closed, _Sock}, #state{} = S) ->
    {stop, normal, S}.

terminate(normal, #state{event_mgr = EventMgrRef}) ->
    gen_event:notify(EventMgrRef, connection_closed),
    ok.

get_tokens(Data, Cont) ->
    get_tokens(Data, Cont, [], 1).

get_tokens(Data, Cont, Res, Line) ->
    case telnet_scanner:token(Cont, Data) of
	{more, Cont1} ->
	    {ok, lists:reverse(Res), Cont1, Line};
	{done, {ok, Token, NewLine}, Rest} ->
	    get_tokens(Rest, [], [Token | Res], NewLine)
    end.
