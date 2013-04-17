-module(mudec_console_handler).
-author('Alexander Svyazin <guybrush@live.ru>').

-behaviour(gen_event).

-export([init/1, handle_event/2]).

-record(state, {parent}).

init([Parent]) ->
    {ok, #state{parent = Parent}}.

handle_event(Token, #state{parent = Parent} = S) ->
    mudec_console:token_received(Parent, Token),
    {ok, S}.
