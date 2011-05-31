all: povray.beam ppov.beam uuid.beam ppov_http.beam

# OPTIONS=-DLOCALHOST

%.beam: %.erl *.hrl
	erlc $(OPTIONS) $< -o $@

clean:
	@rm -f jobs *.log *.access *~ *.beam erl_crash.dump
