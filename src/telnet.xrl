Definitions.

IAC = \xff
DO = \xfd
DONT = \xfe
WILL = \xfb
WONT = \xfc
SB = \xfa
SE = \xf0

Rules.

{IAC}{DO}. : {token, {do, lists:nth(3, TokenChars)}}.
{IAC}{DONT}. : {token, {dont, lists:nth(3, TokenChars)}}.
{IAC}{WILL}. : {token, {will, lists:nth(3, TokenChars)}}.
{IAC}{WONT}. : {token, {wont, lists:nth(3, TokenChars)}}.
{IAC}{SB}.+{IAC}{SE} : {token, get_subnego(TokenChars)}.
{IAC}{IAC} : {token, {char, $\xff}}.
{IAC}. : {token, {command, lists:nth(2, TokenChars)}}.
. : {token, {char, lists:nth(1, TokenChars)}}.

Erlang code.

get_subnego([_, _ | Tail]) ->
  Len = length(Tail),
  Option = lists:nth(1, Tail),
  Data = lists:sublist(Tail, 2, Len - 3),
  {subnego, Option, Data}.