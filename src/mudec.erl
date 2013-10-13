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
    mudec_console_new:connect("materiamagica.com", 23, start_logger("materiamagica_input.bin", "materiamagica_output.bin")).

burning() ->
    start(),
    mudec_console_new:connect("burningmud.com", 4000).

-spec start_logger(InputFile :: string(), OutputFile :: string()) -> pid().
start_logger(InputFile, OutputFile) ->
    {ok, Input} = file:open(InputFile, [write]),
    {ok, Output} = file:open(OutputFile, [write]),
    spawn_link(fun() ->
		       receive
			   {read_bytes, Data} ->
			       io:put_chars(Output, Data);
			   {write_bytes, Data} ->
			       io:put_chars(Input, Data)
		       end
	       end).
