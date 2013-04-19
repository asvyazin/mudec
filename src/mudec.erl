-module (mudec).
-author('Alexander Svyazin <guybrush@live.ru>').

-export ([the_bat/0]).

the_bat() ->
	{ok, _Pid} = mudec_console:start_link("bat.org", 23).