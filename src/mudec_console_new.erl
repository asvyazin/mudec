-module(mudec_console_new).

-export([connect/2, read_input_loop/1, read_network_loop/1]).

-spec connect(inet:ip_address() | inet:hostname(), inet:port_number()) -> any().
connect(Address, Port) ->
    {ok, Pid} = mudec_telnet_connection:start_link(Address, Port),
%    spawn_link(?MODULE, read_input_loop, [Pid]),
    read_network_loop(Pid).

-spec read_network_loop(pid()) -> any().
read_network_loop(Pid) ->
    {ok, Tokens} = mudec_telnet_connection:read_tokens(Pid),
    [process_token(Pid, Token) || Token <- Tokens],
    ?MODULE:read_network_loop(Pid).

process_token(Pid, {will, Option}) ->
    io:format("server sent unknown WILL: ~p~n", [Option]),
    mudec_telnet_connection:send(Pid, {dont, Option});
process_token(Pid, {do, Option}) ->
    io:format("server sent unknown DO: ~p~n", [Option]),
    mudec_telnet_connection:send(Pid, {wont, Option});
process_token(_, Token) when is_list(Token) ->
    io:format("~s~n", [Token]);
process_token(_, Token) ->
    io:format("~p~n", [Token]).

-spec read_input_loop(pid()) -> any().
read_input_loop(Pid) ->
    Input = io:get_line("> "),
    mudec_telnet_connection:send(Pid, Input),
    ?MODULE:read_input_loop(Pid).
