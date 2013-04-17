-module(telnet_tokenizer).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, token/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {sock, transport, cont}).

-define(TOKEN_TIMEOUT, 120000).

start_link(Sock, Transport) ->
    gen_server:start_link(?MODULE, [Sock, Transport], []).

token(Pid) ->
    gen_server:call(Pid, token, ?TOKEN_TIMEOUT).

stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Sock, Transport]) ->
    {ok, #state{sock = Sock, transport = Transport, cont = []}}.

handle_call(token, _From, #state{sock = Sock, transport = Transport, cont = Cont} = S) ->
    {ok, Token, Line, NewCont} = get_token(Sock, Transport, [], Cont),
    {reply, {ok, Token, Line}, S#state{cont = NewCont}}.

get_token(Sock, Transport, Buf, Cont) ->
    case telnet_scanner:token(Cont, Buf) of
	{more, Cont1} ->
	    {ok, Packet} = Transport:recv(Sock, 0),
	    get_token(Sock, Transport, Packet, Cont1);
	{done, {ok, Token, Line}, Rest} ->
	    {ok, RestTokens, NewLine, NewCont} = get_rest_tokens(Rest, [], [], Line),
	    {ok, [Token | RestTokens] ++ [{'$end', NewLine}], NewLine, NewCont}
    end.

get_rest_tokens(Rest, Cont, RestTokens, Line) ->
    case telnet_scanner:token(Cont, Rest) of
	{more, NewCont} ->
	    {ok, lists:reverse(RestTokens), Line, NewCont};
	{done, {ok, Token, NewLine}, NewRest} ->
	    get_rest_tokens(NewRest, [], [Token | RestTokens], NewLine)
    end.

handle_cast(stop, #state{} = S) ->
    {stop, normal, S}.

terminate(normal, #state{}) ->
    ok.
