ERLC = erlc
ERL_COMPILE_FLAGS = +debug_info

ERL_FILES = $(wildcard *.erl)
BEAM_FILES = $(subst .erl,.beam,$(ERL_FILES))

.PHONY: all clean

all: $(BEAM_FILES)

%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) $<

clean:
	$(RM) $(BEAM_FILES)
	rm -f core erl_crash.dump
