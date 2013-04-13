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
    {ok, [{char, _, $1}, {char, _, $2}], _} = telnet_scanner:string("12").

do_test() ->
    {ok, [{do, _, $1}], _} = telnet_scanner:string([?IAC, ?DO, $1]).

dont_test() ->
    {ok, [{dont, _, $1}], _} = telnet_scanner:string([?IAC, ?DONT, $1]).

will_test() ->
    {ok, [{will, _, $1}], _} = telnet_scanner:string([?IAC, ?WILL, $1]).

wont_test() ->
    {ok, [{wont, _, $1}], _} = telnet_scanner:string([?IAC, ?WONT, $1]).

command_test() ->
    {ok, [{command, _, $1}], _} = telnet_scanner:string([?IAC, $1]).

subnego_test() ->
    {ok, [{subnego, _, {$1, "23"}}], _} = telnet_scanner:string([?IAC, ?SB, $1, $2, $3, ?IAC, ?SE]).

iaciac_test() ->
    {ok, [{char, _, $\xff}, {char, _, $1}], _} = telnet_scanner:string([?IAC, ?IAC, $1]).
