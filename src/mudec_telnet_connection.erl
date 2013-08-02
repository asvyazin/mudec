-module(mudec_telnet_connection).

-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_server).

-export([start_link/2, read_tokens/1, set_mode/2, get_mode/1, send/2]).
-export_type([send_token/0, telnet_option/0, telnet_command/0, telnet_subnego_payload/0]).

-type telnet_option() :: integer().
-type telnet_command() :: integer().
-type telnet_subnego_payload() :: string().
-type send_token() :: {do, telnet_option()}
		    | {dont, telnet_option()}
		    | {will, telnet_option()}
		    | {wont, telnet_option()}
		    | {command, telnet_command()}
		    | {subnego, telnet_option(), telnet_subnego_payload()}
		    | string().

-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {socket :: inet:socket(), buffer :: string(), mode :: telnet_packet:mode()}).

-type address() :: inet:ip_address() | inet:hostname().
-spec start_link(Address :: address(), Port :: inet:port_number()) -> {ok, pid()}.
start_link(Address, Port) ->
    gen_server:start_link(?MODULE, [Address, Port], []).

init([Address, Port]) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, [{active, false}, {mode, list}]),
    {ok, #state{socket = Socket, buffer = [], mode = newline}}.

-spec read_tokens(pid()) -> {ok, list(mudec_telnet_reader:telnet_token())}.
read_tokens(Pid) ->
    gen_server:call(Pid, read_tokens, infinity).

-spec set_mode(pid(), telnet_packet:mode()) -> ok.
set_mode(Pid, Mode) ->
    gen_server:cast(Pid, {set_mode, Mode}).

-spec get_mode(pid()) -> telnet_packet:mode().
get_mode(Pid) ->
    gen_server:call(Pid, get_mode).

-spec send(pid(), send_token() | [send_token()]) -> ok.
send(Pid, Tokens) ->
    gen_server:cast(Pid, {send, Tokens}).

handle_call(read_tokens, _Reply, #state{socket = Socket, buffer = Buffer, mode = Mode} = S) ->
    {ok, Tokens, NewBuffer} = mudec_telnet_reader:read_tokens(Socket, Buffer, Mode),
    {reply, {ok, Tokens}, S#state{buffer = NewBuffer}};
handle_call(get_mode, _Reply, #state{mode = Mode} = S) ->
    {reply, {ok, Mode}, S}.

handle_cast({set_mode, Mode}, #state{} = S) ->
    {noreply, S#state{mode = Mode}};
handle_cast({send, Tokens}, #state{socket = Socket} = S) ->
    gen_tcp:send(Socket, mudec_telnet_writer:to_telnet(Tokens)),
    {noreply, S}.