FILES=$(wildcard datapouch/*.lisp) $(wildcard zettelkasten-and-accounting/*.lisp)
ASDF_DEF=datapouch/datapouch.asd zettelkasten-and-accounting/zettelkasten-and-accounting.asd
QUICKLISP=~/lisp/quicklisp

EXEC_NAME=dtpch

all: executable

executable: $(FILES) $(ASDF_DEF)
	sbcl --noinform \
	     --load $(QUICKLISP)/setup.lisp \
	     --load load.lisp \
	     --eval '(zac.main:make-zac "$(EXEC_NAME)" :executable t :compression 9)'

no_compress: $(FILES) $(ASDF_DEF)
	sbcl --noinform \
	     --load $(QUICKLISP)/setup.lisp \
	     --load load.lisp \
	     --eval '(zac.main:make-zac "$(EXEC_NAME)" :executable t)'

debug: $(FILES) $(ASDF_DEF)
	rlwrap sbcl --load $(QUICKLISP)/setup.lisp \
	            --load load.lisp

clean:
	-rm -f $(EXEC_NAME)

.PHONY: all executable no_compress debug clean
