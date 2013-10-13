-module(tcp_connection_proxy).
-behaviour(gen_server).

-type address() :: inet:ip_address() | inet:hostname().
-export_type([address/0]).
-export([read_bytes/1, write_bytes/2, start_link/2, start_link/3, add_listener/2]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {socket :: inet:socket(), event_listeners :: list(pid())}).

-spec start_link(Address :: address(), Port :: inet:port_number()) -> {ok, inet:socket()}.
start_link(Address, Port) ->
    start_link(Address, Port, []).

-spec start_link(Address :: address(), Port :: inet:port_number(), Listeners :: list(pid())) -> {ok, inet:socket()}.
start_link(Address, Port, Listeners) ->
    gen_server:start_link(?MODULE, [Address, Port, Listeners], []).    

-spec read_bytes(Pid :: pid()) -> {ok, string()}.
read_bytes(Pid) ->
    gen_server:call(Pid, read_bytes).

-spec write_bytes(Pid :: pid(), Data :: string()) -> ok.
write_bytes(Pid, Data) ->
    gen_server:cast(Pid, {write_bytes, Data}).

-spec add_listener(Pid :: pid(), Listener :: pid()) -> ok.
add_listener(Pid, Listener) ->
    gen_server:cast(Pid, {add_listener, Listener}).

init([Address, Port, Listeners]) ->
    #state{socket = gen_tcp:connect(Address, Port, [{active, false}, {mode, list}]), event_listeners = Listeners}.

handle_call(read_bytes, _From, State = #state{socket = Sock, event_listeners = Listeners}) ->
    {ok, Data} = gen_tcp:recv(Sock, 0),
    raise_read_bytes_event(Data, Listeners),
    {reply, {ok, Data}, State}.
    
handle_cast({write_bytes, Data}, State = #state{socket = Sock, event_listeners = Listeners}) ->
    raise_write_bytes_event(Data, Listeners),
    ok = gen_tcp:send(Sock, Data),
    {noreply, State};
handle_cast({add_listener, Listener}, State = #state{event_listeners = Listeners}) ->
    {noreply, State#state{event_listeners = [Listener | Listeners]}}.

terminate(normal, _State) ->
    ok.

-spec raise_read_bytes_event(Data :: string(), Listeners :: list(pid())) -> ok.
raise_read_bytes_event(Data, Listeners) ->
    [Pid ! {read_bytes, Data} || Pid <- Listeners],
    ok.

-spec raise_write_bytes_event(Data :: string(), Listeners :: list(pid())) -> ok.
raise_write_bytes_event(Data, Listeners) ->
    [Pid ! {write_bytes, Data} || Pid <- Listeners],
    ok.
