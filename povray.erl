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


render(File) ->
    Ref = make_ref(),
    spawn_link(?MODULE, render, [self(), Ref, File]),
    receive
	{Ref, Result} -> %% {Job, Port}
	    Result
    end.


render(Parent, Ref, File) ->
    UUID = uuid:timestamp(),
    Povray = os:find_executable(?POVRAY),
    FileName = filename:basename(File),
    DirName = filename:dirname(File),

    %% Size = " +W160 +H120",
    %% Size = " +W320 +H240",
    Size = " +W640 +H480",
    %% Size = " +W800 +H600",

    PovArgs = " +A",
    Args = Size ++ PovArgs ++ " " ++ FileName,
    Cmd = Povray ++ Args,
    Job = #job{id=UUID, cmd=Cmd, dirname=DirName},
    run_povray(Parent, Ref, Job). %%UUID, Cmd, DirName).

run_povray(Parent, Ref, #job{id=UUID, cmd=Cmd, dirname=DirName} = Job) ->
    Nice = os:find_executable(?NICE)++" -n19",
    %% io:format("Spawning command: ~p in ~p~n", [Cmd, DirName]),
    Cmd2 = Nice++" "++Cmd++" -D +C "++" 2> ppov.log",  %%% CHEAT WITH +C
    %% Cmd2 = Nice++" "++Cmd++" +C "++" 2> ppov.log",  %%% CHEAT WITH +C
    Port = open_port({spawn, Cmd2}, [{cd, DirName}, exit_status]),
    %% ppov:started(Job, Port),
    Parent ! {Ref, {Job, Port}},
    Exit = receive
	       {Port, {exit_status, E}} ->
		   E
	   end,
    case Exit of
	0 ->
	    ppov:done(UUID, Port);
	
	%% TODO: cf http://www.cs.pitt.edu/~alanjawi/cs449/code/shell/UnixSignals.htm
	Other -> %% TODO: paused ou error
	    error_logger:error_msg("Job ~s exited with code ~p~nCmd: ~p~nDir: ~p~n~n", [uuid:to_string(UUID), Other, Cmd, DirName]),
	    ppov:error(UUID, Port, Other)
    end.
