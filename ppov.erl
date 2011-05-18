-module(ppov).
-author('olivier@biniou.info').

-include("ppov.hrl").

-compile([export_all]).

-export([started/3, done/1, error/2]).
-export([status/0]).

%% Internal exports
-export([boot/0]).

-define(SERVER, ?MODULE).


start() ->
    ppov_http:start(),
    uuid:start(),
    spawn(?MODULE, boot, []).

test() ->
    povray:test().

started(UUID, Cmd, DirName) ->
    Job = #job{id=UUID, cmd=Cmd, dirname=DirName},
    cast({started, Job}).
done(UUID) ->
    cast({done, UUID}).
error(UUID, Error) ->
    cast({error, UUID, Error}).

status() ->
    call(status).

stop() ->
    ?SERVER ! stop.

boot() ->
    {ok, ?JOBS} = dets:open_file(?JOBS, [{keypos, 2}]),
    register(?SERVER, self()),
    loop().

loop() ->
    receive
	{started, Job} ->
	    dets:insert(?JOBS, Job),
	    loop();

	{done, UUID} ->
	    Job = job(UUID),
	    dets:insert(?JOBS, Job#job{status=done}),
	    loop();

	{From, Ref, status} ->
	    dets:foldl(fun display/2, [], ?JOBS),
	    From ! {Ref, ok},
	    loop();
	    
	stop ->
	    dets:close(?JOBS),
	    uuid:stop();
	
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

job(UUID) ->
    [Job] = dets:lookup(?JOBS, UUID),
    Job.
