-module(telnet_tests).
-include_lib("eunit/include/eunit.hrl").

-include("telnet.hrl").

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

telnet_test_transport_start_and_stop_test() ->
    ?assertMatch({ok, _}, telnet_test_transport:start_link()),
    ?assertMatch(ok, telnet_test_transport:stop()),
    ?assertMatch({ok, _}, telnet_test_transport:start_link()),
    ?assertMatch(ok, telnet_test_transport:stop()).

telnet_tokenizer_setup() ->
    {ok, _TPid} = telnet_test_transport:start_link(),
    {ok, Pid} = telnet_tokenizer:start_link(0, telnet_test_transport),
    Pid.

telnet_tokenizer_teardown(Pid) ->
    telnet_tokenizer:stop(Pid),
    telnet_test_transport:stop().

telnet_tokenizer_test_() ->
    {setup,
     fun telnet_tokenizer_setup/0,
     fun telnet_tokenizer_teardown/1,
     fun (Pid) ->
	     [?_test(
		 begin		     
		     telnet_test_transport:packet("12"),
		     ?assertMatch({ok, [{char, _, $1}, {char, _, $2}, {'$end', _}], _}, telnet_tokenizer:token(Pid))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC, ?DO, $3]),
		     ?assertMatch({ok, [{do, _, $3}, {'$end', _}], _}, telnet_tokenizer:token(Pid))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC, ?DONT]),
		     telnet_test_transport:packet("1"),		     
		     ?assertMatch({ok, [{dont, _, $1}, {'$end', _}], _}, telnet_tokenizer:token(Pid))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC]),
		     telnet_test_transport:packet([?IAC]),
		     ?assertMatch({ok, [{char, _, ?IAC}, {'$end', _}], _}, telnet_tokenizer:token(Pid))
		 end)]
     end}.

telnet_parser_with_tokenizer_test_() ->
    {setup,
     fun telnet_tokenizer_setup/0,
     fun telnet_tokenizer_teardown/1,
     fun (Pid) ->
	     [?_test(
		 begin
		     telnet_test_transport:packet("12"),
		     ?assertMatch({ok, ["12"]}, telnet_parser:parse_and_scan({telnet_tokenizer, token, [Pid]}))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC, ?DO, $3]),
		     ?assertMatch({ok, [{do, $3}]}, telnet_parser:parse_and_scan({telnet_tokenizer, token, [Pid]}))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC, ?DONT]),
		     telnet_test_transport:packet("1"),
		     ?assertMatch({ok, [{dont, $1}]}, telnet_parser:parse_and_scan({telnet_tokenizer, token, [Pid]}))
		 end),
	      ?_test(
		 begin
		     telnet_test_transport:packet([?IAC]),
		     telnet_test_transport:packet([?IAC]),    
		     ?assertMatch({ok, ["\xff"]}, telnet_parser:parse_and_scan({telnet_tokenizer, token, [Pid]}))		     
		 end)]
     end}.
