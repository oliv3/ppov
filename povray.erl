-module(povray).
-author('olivier@biniou.info').

-compile([export_all]).

-define(NICE, "nice").
-define(POVRAY, "povray").

-define(TEST, "test/planet/planet.pov").

cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD++"/".
test() ->
    render(cwd()++?TEST).

render(File) ->
    UUID = uuid:timestamp(),
    spawn_link(?MODULE, render, [UUID, File]),
    {ok, uuid:to_string(UUID)}.


%% cf http://www.cs.pitt.edu/~alanjawi/cs449/code/shell/UnixSignals.htm

render(UUID, File) ->
    Povray = os:find_executable(?POVRAY),
    FileName = filename:basename(File),
    DirName = filename:dirname(File),
    Args = " +I" ++ FileName ++ " +O" ++ pov2png(FileName),
    Cmd = Povray ++ Args,
    run_povray(UUID, Cmd, DirName).

run_povray(UUID, Cmd, DirName) ->
    Nice = os:find_executable(?NICE),
    %% io:format("Spawning command: ~p in ~p~n", [Cmd, DirName]),
    Cmd2 = Nice++" "++Cmd++" 2>/dev/null",
    Port = open_port({spawn, Cmd2}, [{cd, DirName}, exit_status]),
    ppov:started(UUID, Cmd, DirName),
    Exit = receive
	       {Port, {exit_status, E}} ->
		   E
	   end,
    case Exit of
	0 ->
	    ppov:done(UUID);
	
	Other -> %% TODO: paused ou error
	    ppov:error(UUID, Other)
    end.


pov2png(File0) ->
    Base = filename:basename(File0, ".pov"),
    Base++".png".
