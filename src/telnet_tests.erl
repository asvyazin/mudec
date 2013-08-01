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

telnet_packet_test_() ->
    [?_assertMatch({ok, "123\r\n", "456"}, telnet_packet:read(undefinite, "123\r\n456", newline)),
     ?_assertMatch({ok, "1", "2"}, telnet_packet:read(undefinite, [$1, ?IAC, ?EOR, $2], eor)),
     ?_assertMatch({ok, "1", "2"}, telnet_packet:read(undefinite, [$1, ?IAC, ?GA, $2], ga)),
     ?_assertMatch({ok, "1\r\n", "2"}, telnet_packet:read(undefinite, [$1, $\r, $\n, ?IAC, ?EOR, $2], eor)),
     ?_assertMatch({ok, "1\r\n", "2"}, telnet_packet:read(undefinite, [$1, $\r, $\n, ?IAC, ?GA, $2], ga)),
     ?_assertMatch({ok, [$1, $\r, $\n, ?IAC, ?EOR], "2"}, telnet_packet:read(undefinite, [$1, $\r, $\n, ?IAC, ?EOR, ?IAC, ?GA, $2], ga)),
     ?_assertMatch({ok, [$1, $\r, $\n, ?IAC, ?GA], "2"}, telnet_packet:read(undefinite, [$1, $\r, $\n, ?IAC, ?GA, ?IAC, ?EOR, $2], eor)),
     ?_assertMatch({ok, [$1, ?IAC, ?EOR, ?IAC, ?GA, $\r, $\n], "2"}, telnet_packet:read(undefinite, [$1, ?IAC, ?EOR, ?IAC, ?GA, $\r, $\n, $2], newline))].

telnet_packet_sock_recv_test_() ->
    {setup,
     fun () ->
	     meck:new(gen_tcp, [unstick]),
	     meck:expect(gen_tcp, recv, 2, meck:seq([meck:val({ok, [$4, ?IAC, ?EOR]}), meck:val({ok, [$5, ?IAC, ?GA]}), meck:val({ok, "6\r\n789"})]))
     end,
     fun (_) -> meck:unload(gen_tcp) end,
     fun (_) ->
	     ?_test(
		begin
		    ?assertMatch({ok, [$1, $2, $3, $4, ?IAC, ?EOR, $5, ?IAC, ?GA, $6, $\r, $\n], "789"}, telnet_packet:read(undefinite, "123", newline)),
		    meck:validate(gen_tcp)
		end)
     end}.

telnet_writer_test_() ->
    [?_assertMatch("12", telnet_writer:to_telnet("12")),
     ?_assertMatch([[?IAC, ?IAC], $1], telnet_writer:to_telnet([?IAC, $1])),
     ?_assertMatch([?IAC, ?DO, $1], telnet_writer:to_telnet({do, $1})),
     ?_assertMatch([?IAC, ?DONT, $1], telnet_writer:to_telnet({dont, $1})),
     ?_assertMatch([?IAC, ?WILL, $1], telnet_writer:to_telnet({will, $1})),
     ?_assertMatch([?IAC, ?WONT, $1], telnet_writer:to_telnet({wont, $1})),
     ?_assertMatch([?IAC, $1], telnet_writer:to_telnet({command, $1})),
     ?_assertMatch([[?IAC, $1], [?IAC, ?DO, $2], [?IAC, ?DONT, $3], [?IAC, ?WILL, $4], [?IAC, ?WONT, $5], [?IAC, ?SB, $6, "78", ?IAC, ?SE], "90"],
		   telnet_writer:to_telnet([{command, $1}, {do, $2}, {dont, $3}, {will, $4}, {wont, $5}, {subnego, $6, "78"}, "90"]))].
