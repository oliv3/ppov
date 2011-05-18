all: povray.beam ppov.beam uuid.beam ppov_http.beam

%.beam: %.erl
	erlc $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump
