Nonterminals telnet chars tokens token.
Terminals char do dont will wont subnego command.
Rootsymbol tokens.
telnet -> do : '$1'.
telnet -> dont : '$1'.
telnet -> will : '$1'.
telnet -> wont : '$1'.
telnet -> subnego : '$1'.
telnet -> command : '$1'.
chars -> char chars : [value_of('$1') | '$2'].
chars -> char : [value_of('$1')].
token -> chars : '$1'.
token -> telnet : {category_of('$1'), value_of('$1')}.
tokens -> token : ['$1'].
tokens -> token tokens : ['$1' | '$2'].

Erlang code.

category_of(Token) ->
  {Res, _, _} = Token,
  Res.

value_of(Token) ->
  {_, _, Res} = Token,
  Res.