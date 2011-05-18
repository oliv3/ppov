-module(povray).
-author('olivier@biniou.info').

-include("ppov.hrl").

-compile([export_all]).

%% TODO: export render/2 and run_povray/3

-define(NICE, "nice").

%% Super hack to close port processes:
%% ln -s `which povray` ppovray
%% Then you can "killall ppovray" :)
-define(POVRAY, "ppovray").

-define(TESTS, [
		"test/planet/planet.pov",
		"test/advanced/landscape/landscape.pov",
		"test/advanced/wineglass/wineglass.pov",
		"test/advanced/chess2/chess2.pov",
		"test/advanced/abyss/abyss.pov",
		"test/advanced/woodbox/woodbox.pov",
		"test/animations/ambient/ambient.ini"
	       ]).

cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD++"/".

test() ->
    [render(cwd()++File) || File <- ?TESTS].

render(File) ->
    UUID = uuid:timestamp(),
    spawn_link(?MODULE, render, [UUID, File]),
    {ok, uuid:to_string(UUID)}.


render(UUID, File) ->
    Povray = os:find_executable(?POVRAY),
    FileName = filename:basename(File),
    DirName = filename:dirname(File),
    Size = " +W640 +H480",
    Args = Size ++ " " ++ FileName,
    Cmd = Povray ++ Args,
    run_povray(UUID, Cmd, DirName).

run_povray(UUID, Cmd, DirName) ->
    Nice = os:find_executable(?NICE)++" -n19",
    %% io:format("Spawning command: ~p in ~p~n", [Cmd, DirName]),
    Cmd2 = Nice++" "++Cmd++" +C "++" 2> ppov.log",  %%% CHEAT WITH +C
    Port = open_port({spawn, Cmd2}, [{cd, DirName}, exit_status]),
    Job = #job{id=UUID, cmd=Cmd, dirname=DirName},
    ppov:started(Job, Port),
    Exit = receive
	       {Port, {exit_status, E}} ->
		   E
	   end,
    case Exit of
	0 ->
	    ppov:done(UUID, Port);
	
	%% TODO: cf http://www.cs.pitt.edu/~alanjawi/cs449/code/shell/UnixSignals.htm
	Other -> %% TODO: paused ou error
	    ppov:error(UUID, Other)
    end.
