-record(job, {id,               %% UUID
	      cmd, dirname,     %% Command and directory
	      status=running    %% |paused|done|{error, exit_status}
	     }).

-define(JOBS, jobs). %% jobs DETS table
