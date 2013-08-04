-module(mudec_console_new).

-author('Alexander Svyazin <guybrush@live.ru>').

-export([connect/2, read_input_loop/1, read_network_loop/1]).

-include("telnet.hrl").

-spec connect(inet:ip_address() | inet:hostname(), inet:port_number()) -> any().
connect(Address, Port) ->
    {ok, Pid} = mudec_telnet_connection:start_link(Address, Port),
    spawn_link(?MODULE, read_input_loop, [Pid]),
    read_network_loop(Pid).

-spec read_network_loop(pid()) -> any().
read_network_loop(Pid) ->
    {ok, Tokens} = mudec_telnet_connection:read_tokens(Pid),
    [process_token(Pid, Token) || Token <- Tokens],
    ?MODULE:read_network_loop(Pid).

process_token(Pid, {will, ?SUPPRESS_GO_AHEAD}) ->
    mudec_telnet_connection:send(Pid, {dont, ?SUPPRESS_GO_AHEAD});
process_token(Pid, {will, ?END_OF_RECORD}) ->
    mudec_telnet_connection:send(Pid, {do, ?END_OF_RECORD});
process_token(Pid, {will, Option}) ->
    io:format("server sent unknown WILL: ~p~n", [Option]),
    mudec_telnet_connection:send(Pid, {dont, Option});
process_token(Pid, {do, Option}) ->
    io:format("server sent unknown DO: ~p~n", [Option]),
    mudec_telnet_connection:send(Pid, {wont, Option});
process_token(Pid, {command, ?EOR}) ->
    io:format("EOR mode ON~n", []),
    mudec_telnet_connection:set_mode(Pid, eor);    
process_token(_, Token) when is_list(Token) ->
    io:format("~s~n", [Token]);
process_token(_, Token) ->
    io:format("~p~n", [Token]).

-spec read_input_loop(pid()) -> any().
read_input_loop(Pid) ->
    Input = io:get_line(""),
    mudec_telnet_connection:send(Pid, Input),
    ?MODULE:read_input_loop(Pid).
