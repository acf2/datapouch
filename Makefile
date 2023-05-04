FILES=$(wildcard datapouch/*.lisp)
ASDF_DEF=datapouch/datapouch.asd
QUICKLISP=~/lisp/quicklisp

EXEC=dtpch
NAME=datapouch
VERSION=0.2

PACK := $(NAME)-$(VERSION)

executable: $(FILES) $(ASDF_DEF)
	sbcl --noinform \
	     --load $(QUICKLISP)/setup.lisp \
	     --load load.lisp \
	     --eval '(d.main:make-image "$(EXEC)" :executable t :compression 9)'

no_compress: $(FILES) $(ASDF_DEF)
	sbcl --noinform \
	     --load $(QUICKLISP)/setup.lisp \
	     --load load.lisp \
	     --eval '(d.main:make-image "$(EXEC)" :executable t)'

debug: $(FILES) $(ASDF_DEF)
	rlwrap sbcl --load $(QUICKLISP)/setup.lisp \
	            --load load.lisp

test: $(FILES) $(ASDF_DEF)
	sbcl --load $(QUICKLISP)/setup.lisp \
	     --load load.lisp \
	     --eval '(d.main:main)'

deploy: executable
	mkdir -p $(PACK)
	mv $(EXEC) $(PACK)/

test_deploy: no_compress
	mkdir -p $(PACK)
	mv $(EXEC) $(PACK)/

clean:
	-rm -f $(EXEC)
	-rm -rf $(PACK)

.PHONY: all exec deploy clean
