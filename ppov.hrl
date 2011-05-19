-record(job, {id,               %% UUID
	      cmd, dirname,     %% Command and directory
	      status=running    %% |paused|done|{error, exit_status}
	     }).

-define(JOBS, jobs). %% jobs DETS table

-ifdef(LOCALHOST).
-define(CONTROL_HOST, "control.ppov.localhost").
-define(DATA_HOST,    "data.ppov.localhost").
-else.
-define(CONTROL_HOST, "control.ppov.home.biniou.net").
-define(DATA_HOST,    "data.ppov.home.biniou.net").
-endif.
