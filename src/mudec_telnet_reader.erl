-module(mudec_telnet_reader).

-author('Alexander Svyazin <guybrush@live.ru>').

-export([read_tokens/3]).
-export_type([telnet_token/0]).

-type telnet_token() :: string()
		      | {do, char()}
		      | {dont, char()}
		      | {will, char()}
		      | {wont, char()}
		      | {command, char()}
		      | {subnego, {char(), string()}}.

-spec read_tokens(Sock :: inet:socket(), Data :: string(), Mode :: telnet_packet:packet_mode()) -> {ok, list(telnet_token()), string()}.
read_tokens(Sock, Data, Mode) ->
    {ok, Packet, NewData} = telnet_packet:read(Sock, Data, Mode),
    {ok, Chunks, _} = telnet_scanner:string(Packet),
    {ok, Tokens} = telnet_parser:parse(Chunks ++ [{'$end', 1}]),
    {ok, Tokens, NewData}.
