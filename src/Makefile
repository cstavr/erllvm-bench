#DIRS = $(wildcard */)
DIRS = shootout #XXX: Work with shootout for now!

.PHONY: all clean

all:
	for d in $(DIRS); do (cd $$d && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@);done

clean:
	for d in $(DIRS); do (cd $$d && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@); done
