-module(ppov).
-author('olivier@biniou.info').


-compile([export_all]).

-export([started/1, done/1, error/2]).
-export([status/0]).

%% Internal exports
-export([boot/0]).

-define(SERVER, ?MODULE).

-record(job, {id,               %% UUID
	      status=running    %% |paused|done|{error, exit_status}
	     }).
-define(JOBS, jobs).

start() ->
    uuid:start(),
    spawn(?MODULE, boot, []).

test() ->
    povray:test().

started(UUID) ->
    Job = #job{id=UUID},
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
	    [Job] = dets:lookup(?JOBS, UUID),
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

display(#job{id=UUID, status=Status}, Acc) ->
    io:format("~s status: ~p~n", [uuid:to_string(UUID), Status]),
    Acc.
