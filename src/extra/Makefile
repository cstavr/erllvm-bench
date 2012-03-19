EBIN_D     = $(EBIN_DIR)/extra
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(patsubst %.erl,$(EBIN_D)/%.beam,$(ERL_FILES))

.PHONY: all clean

all: $(BEAM_FILES)

$(EBIN_D)/%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN_D) $<

$(BEAM_FILES): | $(EBIN_D)

$(EBIN_D):
	mkdir -p $(EBIN_D)

clean:
	$(RM) -rf $(EBIN_D)
