Definitions.

IAC = \xff
DO = \xfd
DONT = \xfe
WILL = \xfb
WONT = \xfc
SB = \xfa
SE = \xf0

Rules.

{IAC}{DO}. : {token, {do, TokenLine, lists:nth(3, TokenChars)}}.
{IAC}{DONT}. : {token, {dont, TokenLine, lists:nth(3, TokenChars)}}.
{IAC}{WILL}. : {token, {will, TokenLine, lists:nth(3, TokenChars)}}.
{IAC}{WONT}. : {token, {wont, TokenLine, lists:nth(3, TokenChars)}}.
{IAC}{SB}.+{IAC}{SE} : {token, {subnego, TokenLine, get_subnego(TokenChars)}}.
{IAC}{IAC} : {token, {char, TokenLine, $\xff}}.
{IAC}. : {token, {command, TokenLine, lists:nth(2, TokenChars)}}.
. : {token, {char, TokenLine, lists:nth(1, TokenChars)}}.

Erlang code.

get_subnego([_, _ | Tail]) ->
  Len = length(Tail),
  Option = lists:nth(1, Tail),
  Data = lists:sublist(Tail, 2, Len - 3),
  {Option, Data}.