all: povray.beam ppov.beam uuid.beam ppov_http.beam

# OPTIONS=-DLOCALHOST

%.beam: %.erl *.hrl
	erlc $(OPTIONS) $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump
