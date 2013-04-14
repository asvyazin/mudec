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

incomplete_parsing_test() ->
    {more, Cont0} = telnet_scanner:token([], []),
    {more, Cont} = telnet_scanner:token(Cont0, [?IAC]),
    {done, {ok, {char, _, ?IAC}, _}, []} = telnet_scanner:token(Cont, [?IAC]),
    {done, {ok, {char, _, ?IAC}, _}, [?IAC]} = telnet_scanner:token([], [?IAC, ?IAC, ?IAC]),
    {more, Cont1} = telnet_scanner:token([], [?IAC, ?DONT]),
    {done, {ok, {dont, _, $1}, _}, []} = telnet_scanner:token(Cont1, "1").

parser_telnet_test() ->
    ?assertMatch({ok, [{do, $1}]}, telnet_parser:parse([{do, 1, $1}, {'$end', 1}])),
    ?assertMatch({ok, [{dont, $1}]}, telnet_parser:parse([{dont, 1, $1}, {'$end', 1}])),
    ?assertMatch({ok, [{will, $1}]}, telnet_parser:parse([{will, 1, $1}, {'$end', 1}])),
    ?assertMatch({ok, [{wont, $1}]}, telnet_parser:parse([{wont, 1, $1}, {'$end', 1}])),
    ?assertMatch({ok, [{subnego, {$1, "23"}}]}, telnet_parser:parse([{subnego, 1, {$1, "23"}}, {'$end', 1}])),
    ?assertMatch({ok, [{command, $1}]}, telnet_parser:parse([{command, 1, $1}, {'$end', 1}])).
    
parser_text_test() ->
    ?assertMatch({ok, ["12"]}, telnet_parser:parse([{char, 1, $1}, {char, 1, $2}, {'$end', 1}])),
    ?assertMatch({ok, ["12", {do, $3}]}, telnet_parser:parse([{char, 1, $1}, {char, 1, $2}, {do, 1, $3}, {'$end', 1}])),
    ?assertMatch({ok, ["12", {do, $3}, "45"]}, telnet_parser:parse([{char, 1, $1}, {char, 1, $2}, {do, 1, $3}, {char, 1, $4}, {char, 1, $5}, {'$end', 1}])).

telnet_tokenizer_test() ->
    {ok, _TPid} = telnet_test_transport:start_link(),
    {ok, Pid} = telnet_tokenizer:start_link(0, telnet_test_transport),
    telnet_test_transport:packet("12"),
    telnet_test_transport:packet([?IAC, ?DO, $3]),
    telnet_test_transport:packet([?IAC, ?DONT]),
    telnet_test_transport:packet("1"),
    telnet_test_transport:packet([?IAC]),
    telnet_test_transport:packet([?IAC]),
    ?assertMatch({ok, [{char, _, $1}, {char, _, $2}], _}, telnet_tokenizer:token(Pid)),
    ?assertMatch({ok, [{do, _, $3}], _}, telnet_tokenizer:token(Pid)),
    ?assertMatch({ok, [{dont, _, $1}], _}, telnet_tokenizer:token(Pid)),
    ?assertMatch({ok, [{char, _, ?IAC}], _}, telnet_tokenizer:token(Pid)).

