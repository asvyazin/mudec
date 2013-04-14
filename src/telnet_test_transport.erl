-module(telnet_test_transport).
-author('Alexander Svyazin <guybrush@live.ru>').
-behaviour(gen_server).

-export([start_link/0, recv/2, packet/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {packets}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{packets = queue:new()}}.

recv(_Sock, 0) ->
    gen_server:call(?MODULE, recv).

packet(Packet) ->
    gen_server:cast(?MODULE, {packet, Packet}).

handle_call(recv, _From, #state{packets = Packets} = State) ->
    {{value, P0}, NewPackets} = queue:out(Packets),
    {reply, {ok, P0}, State#state{packets = NewPackets}}.

handle_cast({packet, Packet}, #state{packets = Packets}) ->
    {noreply, #state{packets = queue:in(Packet, Packets)}}.
