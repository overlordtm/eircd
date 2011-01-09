.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = eircd

all: compile

compile: ${MODS:%=%.beam}

clean: 
	rm -rf *.beam erl_crash.dump
