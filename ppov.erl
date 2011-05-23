-module(ppov).
-author('olivier@biniou.info').

-include("ppov.hrl").

-compile([export_all]).

-export([start/0, stop/0, resume/0]).
-export([started/2, done/2, error/2]).
-export([add/1, status/0, info/0]).

-define(MAX_JOBS, 4).

%% TODO:
%% (ppov@localhost)1> ppov: unhandled message {error,<<131,5,117,66,130,246,17,224,166,202,0,0,0,0,0,
%%                                  0>>,
%%                                143}

%% Tests
-export([test/0]).
-define(TESTS, [
		"test/planet/planet.pov",
		"test/advanced/landscape/landscape.pov",
		"test/advanced/wineglass/wineglass.pov",
		"test/advanced/chess2/chess2.pov",
		"test/advanced/abyss/abyss.pov",
		"test/advanced/woodbox/woodbox.pov",
		"test/animations/ambient/ambient.ini"
	       ]).

%% Internal exports
-export([boot/0]).

-define(SERVER, ?MODULE).
-record(state, {
	  ports=ets:new(ports, [protected]),
	  waiting=ets:new(waiting, [ordered_set, protected]),
	  serial=0
	 }).


start() ->
    ppov_http:start(),
    uuid:start(),
    spawn(?MODULE, boot, []).


started(Job, Port) ->
    cast({started, Job, Port}).
done(UUID, Port) ->
    cast({done, UUID, Port}).
error(UUID, Error) -> %% TODO add Port
    cast({error, UUID, Error}).

add(File) ->
    call({add, File}).

info() ->
    call(info).
status() ->
    call(status).

stop() ->
    call(stop),
    uuid:stop(),
    %% TODO: ppov_http:stop() ?
    init:stop().

resume() ->
    call(resume).

boot() ->
    {ok, ?JOBS} = dets:open_file(?JOBS, [{keypos, 2}]),
    register(?SERVER, self()),
    loop(#state{}).

loop(#state{ports=Tid, waiting=WaitTid, serial=S} = State) ->
    receive
	{From, Ref, {add, File}} ->
	    N = ets:info(Tid, size),
	    if
		N < ?MAX_JOBS ->
		    start_job(Tid, File),
		    From ! {Ref, {added, File}},
		    loop(State);
		true ->
		    %% io:format("Add file to waiting queue: ~p~n", [File]),
		    ets:insert(WaitTid, {S, File}),
		    From ! {Ref, {waiting, File}},
		    loop(State#state{serial=S+1})
	    end;

	{done, UUID, Port} ->
	    Job = job(UUID),
	    dets:insert(?JOBS, Job#job{status=done}),
	    ets:delete(Tid, Port),

	    %% Start a waiting job
	    case ets:tab2list(WaitTid) of
		[] ->
		    ok;
		[{Serial, File}|_] ->
		    %% io:format("Start waiting job: ~p, ~p~n", [Serial, File]),
		    ets:delete(WaitTid, Serial),
		    start_job(Tid, File)
	    end,
	    loop(State);

	{From, Ref, info} ->
	    dets:foldl(fun display/2, [], ?JOBS),
	    From ! {Ref, ok},
	    loop(State);

	{From, Ref, status} ->
	    Running = ets:info(Tid, size),
	    Waiting = ets:info(WaitTid, size),
	    io:format("Running: ~p Waiting: ~p~n", [Running, Waiting]),
	    From ! {Ref, ok},
	    loop(State);

	{From, Ref, stop} ->
	    %% io:format("Remaining ports: ~p~n", [ets:tab2list(Tid)]),
	    [catch port_close(Port) || {Port} <- ets:tab2list(Tid)],
	    dets:foldl(fun pause/2, [], ?JOBS),
	    dets:close(?JOBS),
	    From ! {Ref, stopped};

	{From, Ref, resume} ->
	    dets:foldl(fun resume/2, [], ?JOBS),
	    From ! {Ref, ok},
	    loop(State);
	
	Other ->
	    io:format("~p: unhandled message ~p~n", [?MODULE, Other])
    end.

cast(Msg) ->
    ?SERVER ! Msg.
call(Msg) ->
    Ref = make_ref(),
    ?SERVER ! {self(), Ref, Msg},
    receive
	{Ref, Result} ->
	    Result
    end.


start_job(Tid, File) ->
    {Job, Port} = povray:render(File),
    dets:insert(?JOBS, Job),
    ets:insert(Tid, {Port}).


display(#job{id=UUID, cmd=Cmd, dirname=DirName, status=Status}, Acc) ->
    io:format("~s status: ~p command:~s path: ~s~n",
	      [uuid:to_string(UUID), Status, Cmd, DirName]),
    Acc.


pause(#job{status=running} = Job, Acc) ->
    dets:insert(?JOBS, Job#job{status=paused}),
    Acc;
pause(_Job, Acc) ->
    Acc.


%% FIXME: surement broken avec le system de queue
%% TODO: s/id/uuid in the job record ? --oliv3
resume(#job{id=UUID, status=paused, cmd=Cmd, dirname=DirName} = Job, Acc) ->
    %% povray:run_povray(UUID, Cmd, DirName),
    spawn_link(povray, run_povray, [UUID, Cmd, DirName]),
    dets:insert(?JOBS, Job#job{status=running}),
    Acc;
resume(_Job, Acc) ->
    Acc.


job(UUID) ->
    [Job] = dets:lookup(?JOBS, UUID),
    Job.


%% Tests
cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD++"/".

test() ->
    [add(cwd()++File) || File <- ?TESTS].
