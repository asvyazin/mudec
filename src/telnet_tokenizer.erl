-module(telnet_tokenizer).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/1, token/1]).

-export([init/1, handle_call/3]).

-record(state, {sock, transport, buf}).

start_link(Sock, Transport) ->
    gen_server:start_link(?MODULE, [Sock, Transport], []).

token(Pid) ->
    gen_server:call(Pid, token).

init([Sock, Trnsport]) ->
    {ok, #state{sock = Sock, transport = Transport, buf = []}}.

handle_call(token, _From, #state{sock = Sock, transport = Transport, buf = Buf} = S) ->
    {ok, Token, NewBuf} = get_token(Sock, Transport, Buf, []),
    {reply, {ok, Token}, S#state{buf = NewBuf}}.

get_token(Sock, Transport, Buf, Cont) ->
    case telnet_scanner:token(Cont, Buf) of
	{more, Cont1} ->
	    {ok, Packet} = Transport:recv(Sock, 0),
	    get_token(Sock, Transport, Buf ++ Packet, Cont1);
	{done, {ok, Token, _}, Rest} ->
	    {ok, Token, Rest}
    end.
