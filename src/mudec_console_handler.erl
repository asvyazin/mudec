-module(mudec_console_handler).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2]).

-record(state, {parent}).

init([Parent]) ->
    {ok, #state{parent = Parent}}.

handle_event({token, Token}, #state{parent = Parent} = S) ->
    mudec_console:token_received(Parent, Token),
    {ok, S};
handle_event(connection_closed, #state{parent = Parent}) ->
    mudec_console:stop(Parent),
    remove_handler.

terminate(remove_handler, #state{}) ->
    ok.
