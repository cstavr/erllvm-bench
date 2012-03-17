ifndef EBIN
EBIN = ebin
endif

ifndef SRC
SRC = src
endif

TARGETS= bm.beam w_estone.beam estone.beam prettypr.beam barnes.beam \
	 life.beam ring.beam huff.beam smith.beam decode.beam \
	 qsort.beam length_c.beam length.beam length_u.beam \
	 fib.beam tak.beam nucleic.beam yaws_html.beam nrev.beam \
	 pseudoknot.beam stable.beam float_bm.beam fun_bm.beam \
	 freq_bm.beam call_tail_bm.beam call_bm.beam\
   bs_sum_bm.beam bs_simple_bm.beam bs_bm.beam bin_to_term_bm.beam


# ----------------------------------------------------
# Targets
# ----------------------------------------------------
:all

all: $(TARGETS)

%.beam: %.erl 
	`which erlc` $< 

clean:
	rm -f *.beam
	rm -f core erl_crash.dump

