all: povray.beam ppov.beam uuid.beam

%.beam: %.erl
	erlc $< -o $@

clean:
	@rm -f *~ *.beam erl_crash.dump
