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

process_token(Pid, {will, Option}) ->
    mudec_telnet_connection:send(Pid, {will(Option), Option});
process_token(Pid, {do, Option}) ->
    mudec_telnet_connection:send(Pid, {do(Option), Option});
process_token(Pid, {command, ?EOR}) ->
    lager:info("First EOR reveived, enabling EOR mode..."),
    mudec_telnet_connection:set_mode(Pid, eor);
process_token(Pid, {command, ?GA}) ->
    lager:info("GA received, enabling GA mode..."),
    mudec_telnet_connection:set_mode(Pid, ga);
process_token(_, Token) when is_list(Token) ->
    io:format("~s~n", [Token]);
process_token(_, Token) ->
    lager:warning("Unknown token: ~p", [Token]).

-spec read_input_loop(pid()) -> any().
read_input_loop(Pid) ->
    Input = io:get_line(""),
    mudec_telnet_connection:send(Pid, Input),
    ?MODULE:read_input_loop(Pid).

will(?SUPPRESS_GO_AHEAD) ->
    lager:info("Server wants to suppress GA... Don't do it!"),
    dont;
will(?END_OF_RECORD) ->
    lager:info("Server wants to use EOR... It's good!"),
    do;
will(?TERMINAL_TYPE) ->
    lager:info("Server wants to use TERMINAL-TYPE... We don't support it yet."),
    dont;
will(?TELOPT_MSSP) ->
    lager:info("Server wants to use MSSP... We don't support it yet."),
    dont;
will(?TELOPT_MCCP) ->
    lager:info("Server wants to use MCCP... We don't support it yet."),
    dont;
will(?TELOPT_MSP) ->
    lager:info("Server wants to use MSP... We don't support it yet."),
    dont;
will(?TELOPT_MXP) ->
    lager:info("Server wants to use MXP... We don't support it yet."),
    dont;
will(?TELOPT_GMCP) ->
    lager:info("Server wants to use GMCP... We don't support it yet."),
    dont;
will(Option) ->
    lager:warning("Server wants to use unknown option: ~p", [Option]),
    dont.

do(?NAWS) ->
    lager:info("Server wants to enable NAWS... We don't support it yet."),
    wont;
do(?NEW_ENVIRON) ->
    lager:info("Server wants to enable NEW-ENVIRON... We don't support it yet."),
    wont;
do(?CHARSET) ->
    lager:info("Server wants to enable CHARSET option... We don't support it yet."),
    wont;
do(Option) ->
    lager:warning("Server wants to enable unknown option: ~p", [Option]),
    wont.
