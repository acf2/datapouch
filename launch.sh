sbcl --noinform \
     --non-interactive \
     --load ~/lisp/quicklisp/setup.lisp \
     --load load.lisp \
     --eval '(datapouch.main:main)'
