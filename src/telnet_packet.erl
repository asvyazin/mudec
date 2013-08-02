-module(telnet_packet).

-author('Alexander Svyazin <guybrush@live.ru>').

-include("telnet.hrl").

-export([read/3]).
-export_type([packet/0, buffer/0, packet_mode/0]).

-type packet() :: string().
-type buffer() :: string().
-type packet_mode() :: newline | eor | ga.

-spec read(Sock :: inet:socket(), Data :: buffer(), Mode :: packet_mode()) -> {ok, packet(), buffer()}.
read(Sock, Data, ga) ->
    readUpTo(Sock, Data, [?IAC, ?GA], true);
read(Sock, Data, newline) ->
    readUpTo(Sock, Data, "\r\n", false);
read(Sock, Data, eor) ->
    readUpTo(Sock, Data, [?IAC, ?EOR], true).

readUpTo(Sock, Data, Pattern, SkipPattern) ->
    case string:str(Data, Pattern) of
	0 ->
	    recvAndReadUpTo(Sock, Data, Pattern, SkipPattern);
	N ->
	    Left = string:substr(Data, 1, N - 1),
	    Right = string:substr(Data, N + length(Pattern)),
	    case Left of
		[] ->
		    recvAndReadUpTo(Sock, Right, Pattern, SkipPattern);
		_ ->
		    Packet = case SkipPattern of
				 true ->
				     Left;
				 _ ->
				     Left ++ Pattern
			     end,
		    {ok, Packet, Right}
	    end
    end.

recvAndReadUpTo(Sock, Data, Pattern, SkipPattern) ->
    {ok, MoreData} = gen_tcp:recv(Sock, 0),
    readUpTo(Sock, Data ++ MoreData, Pattern, SkipPattern).
