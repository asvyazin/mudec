-module(telnet_tests).
-include_lib("eunit/include/eunit.hrl").

-define(IAC, $\xff).
-define(DO, $\xfd).
-define(DONT, $\xfe).
-define(WILL, $\xfb).
-define(WONT, $\xfc).
-define(SB, $\xfa).
-define(SE, $\xf0).

chars_test() ->
    {ok, [{char, $1}, {char, $2}], _} = telnet:string("12").

do_test() ->
    {ok, [{do, $1}], _} = telnet:string([?IAC, ?DO, $1]).

dont_test() ->
    {ok, [{dont, $1}], _} = telnet:string([?IAC, ?DONT, $1]).

will_test() ->
    {ok, [{will, $1}], _} = telnet:string([?IAC, ?WILL, $1]).

wont_test() ->
    {ok, [{wont, $1}], _} = telnet:string([?IAC, ?WONT, $1]).

command_test() ->
    {ok, [{command, $1}], _} = telnet:string([?IAC, $1]).

subnego_test() ->
    {ok, [{subnego, $1, "23"}], _} = telnet:string([?IAC, ?SB, $1, $2, $3, ?IAC, ?SE]).

iaciac_test() ->
    {ok, [{char, $\xff}, {char, $1}], _} = telnet:string([?IAC, ?IAC, $1]).
