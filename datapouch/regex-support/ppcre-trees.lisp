;;;; ppcre-trees.lisp


(in-package :datapouch.regex-support)


(defun ppcre-sequence? (seq)
  (and (listp seq)
       (atom (first seq))
       (eq (first seq) :sequence)))


(defun ppcre-alternation? (alt)
  (and (listp alt)
       (atom (first alt))
       (eq (first alt) :alternation)))


(defun ppcre-make-sequence (obj)
  (if (ppcre-sequence? obj)
    obj
    (list :sequence obj)))


(defun ppcre-make-alternation (obj)
  (if (ppcre-alternation? obj)
    obj
    (list :alternation obj)))


(defun ppcre-make-two (make-type-fun one another)
  (let ((one-tree (funcall make-type-fun one))
        (another-tree (funcall make-type-fun another)))
    (append one-tree (rest another-tree))))


(defun ppcre-sequence-two (one another)
  (ppcre-make-two #'ppcre-make-sequence one another))


(defun ppcre-alter-two (one another)
  (ppcre-make-two #'ppcre-make-alternation one another))
