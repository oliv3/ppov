-module(povray).
-author('olivier@biniou.info').

-compile([export_all]).


-define(POVRAY, "povray").

test() ->
    render("test/planet.pov").

render(File) ->
    Ref = make_ref(),
    spawn_link(?MODULE, render, [{self(), Ref}, File]),
    {ok, Ref}.


render({Parent, Ref}, File) ->
    Povray = os:find_executable(?POVRAY),
    FileName = filename:basename(File),
    DirName = filename:dirname(File),
    Args = " +I" ++ FileName ++ " 2>/dev/null",
    Cmd = Povray ++ Args,
    %% io:format("Spawning command: ~p in ~p~n", [Cmd, DirName]),
    Port = open_port({spawn, Cmd}, [{cd, DirName}, exit_status]),
    Exit = receive
	       {Port, {exit_status, E}} ->
		   E
	   end,
    Result = case Exit of
		 0 ->
		     ok;
		 _Other ->
		     failed
	     end,
    Parent ! {Ref, Result}.
