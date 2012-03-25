.PHONY: all clean

all:
	@(cd small/ && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@)
	@(cd extra/ && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@)
	@(cd dons/  && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@)
	@(cd shootout/  && $(MAKE) EBIN_DIR=../$(EBIN_DIR) ERLC=$(ERLC) ERL_COMPILE_FLAGS=$(ERL_COMPILE_FLAGS) $@)

clean:
	@(cd small/ && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@)
	@(cd extra/ && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@)
	@(cd dons/  && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@)
	@(cd shootout/  && $(MAKE) EBIN_DIR=../$(EBIN_DIR) $@)
