-module(telnet_writer).
-author('Alexander Svyazin <guybrush@live.ru>').

-include("telnet.hrl").

-export([to_telnet/1]).

to_telnet(?IAC) ->
    [?IAC, ?IAC];
to_telnet({do, C}) ->
    [?IAC, ?DO, C];
to_telnet({dont, C}) ->
    [?IAC, ?DONT, C];
to_telnet({will, C}) ->
    [?IAC, ?WILL, C];
to_telnet({wont, C}) ->
    [?IAC, ?WONT, C];
to_telnet({command, C}) ->
    [?IAC, C];
to_telnet({subnego, C, Data}) ->
    [?IAC, ?SB, C, Data, ?IAC, ?SE];
to_telnet(C) when is_integer(C) ->
    C;
to_telnet(L) when is_list(L) ->
    list_to_telnet(L).

list_to_telnet(L) ->
    lists:map(fun to_telnet/1, L).
