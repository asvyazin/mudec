-module (mudec).
-author('Alexander Svyazin <guybrush@live.ru>').

-export ([the_bat/0, materiamagica/0]).

the_bat() ->
    mudec_console_new:connect("bat.org", 23).

materiamagica() ->
    mudec_console_new:connect("materiamagica.com", 23).
