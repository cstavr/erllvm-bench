ERLC = erlc
ERL_COMPILE_FLAGS = +debug_info
EBIN_DIR   = ebin
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(subst .erl,.beam,$(ERL_FILES))

.PHONY: all clean distclean

all: $(BEAM_FILES)
	@(cd src && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@)

%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o $(EBIN_DIR) $<

clean:
	$(RM) ebin/$(BEAM_FILES)
	@(cd src && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@)

distclean: clean
	$(RM) -I diagrams/* results/*
