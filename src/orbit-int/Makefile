## orbit-int Makefile
##
## Author: Patrick Maier <P.Maier@hw.ac.uk>
## Modified by: Yiannis Tsiouris <gtsiour@softlab.ntua.gr>
##

EBIN_D    = $(EBIN_DIR)/orbit-int
ERL_FILES  = $(wildcard *.erl)
BEAM_FILES = $(patsubst %.erl,$(EBIN_D)/%.beam,$(ERL_FILES))

.PHONY: all clean

all: $(BEAM_FILES)

$(EBIN_D)/%.beam: %.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o $(EBIN_D) $<

$(BEAM_FILES): | $(EBIN_D)

$(EBIN_D):
	mkdir -p $(EBIN_D)

clean:
	$(RM) -rf $(EBIN_D)
