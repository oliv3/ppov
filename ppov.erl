-module(ppov).
-author('olivier@biniou.info').

-include("ppov.hrl").

-compile([export_all]).

-export([start/0, stop/0, resume/0]).
-export([started/2, done/2, error/2]).
-export([status/0]).

%% Tests
-export([test/0]).

%% Internal exports
-export([boot/0]).

-define(SERVER, ?MODULE).
-record(state, {ports=[]}).


start() ->
    ppov_http:start(),
    uuid:start(),
    spawn(?MODULE, boot, []).

test() ->
    povray:test().

started(Job, Port) ->
    cast({started, Job, Port}).
done(UUID, Port) ->
    cast({done, UUID, Port}).
error(UUID, Error) -> %% TODO add Port
    cast({error, UUID, Error}).

status() ->
    call(status).

stop() ->
    call(stop),
    init:stop().

resume() ->
    call(resume).

boot() ->
    {ok, ?JOBS} = dets:open_file(?JOBS, [{keypos, 2}]),
    register(?SERVER, self()),
    loop(#state{}).

loop(#state{ports=Ports} = State) ->
    receive
	{started, Job, Port} ->
	    dets:insert(?JOBS, Job),
	    loop(State#state{ports=[Port|Ports]});

	{done, UUID, Port} ->
	    Job = job(UUID),
	    dets:insert(?JOBS, Job#job{status=done}),
	    loop(State#state{ports=Ports--[Port]});

	{From, Ref, status} ->
	    dets:foldl(fun display/2, [], ?JOBS),
	    From ! {Ref, ok},
	    loop(State);

	{From, Ref, stop} ->
	    io:format("Closing pending ports: ~p~n", [Ports]),
	    [port_close(Port) || Port <- Ports],
	    dets:foldl(fun pause/2, [], ?JOBS),
	    dets:close(?JOBS),
	    uuid:stop(),
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


display(#job{id=UUID, cmd=Cmd, dirname=DirName, status=Status}, Acc) ->
    io:format("~s status: ~p command:~s path: ~s~n",
	      [uuid:to_string(UUID), Status, Cmd, DirName]),
    Acc.


pause(#job{status=running} = Job, Acc) ->
    dets:insert(?JOBS, Job#job{status=paused}),
    Acc;
pause(_Job, Acc) ->
    Acc.


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
