-module(povray).
-author('olivier@biniou.info').

-compile([export_all]).

-define(NICE, "nice").
-define(POVRAY, "povray").

test() ->
    render("test/planet.pov").

render(File) ->
    UUID = uuid:timestamp(),
    spawn_link(?MODULE, render, [UUID, File]),
    ppov:started(UUID),
    {ok, uuid:to_string(UUID)}.


%% cf http://www.cs.pitt.edu/~alanjawi/cs449/code/shell/UnixSignals.htm

render(UUID, File) ->
    Nice = os:find_executable(?NICE),
    Povray = os:find_executable(?POVRAY),
    FileName = filename:basename(File),
    DirName = filename:dirname(File),
    Args = " +I" ++ FileName ++ " 2>/dev/null",
    Cmd = Nice ++ " " ++ Povray ++ Args,
    %% io:format("Spawning command: ~p in ~p~n", [Cmd, DirName]),
    Port = open_port({spawn, Cmd}, [{cd, DirName}, exit_status]),
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
