-module (mudec).
-author('Alexander Svyazin <guybrush@live.ru>').

-export ([the_bat/0, materiamagica/0, burning/0]).

start() ->
    lager:start(),
    lager:set_loglevel(lager_console_backend, info).

the_bat() ->
    start(),
    mudec_console_new:connect("bat.org", 23).

materiamagica() ->
    start(),
    mudec_console_new:connect("materiamagica.com", 23).

burning() ->
    start(),
    mudec_console_new:connect("burningmud.com", 4000).
