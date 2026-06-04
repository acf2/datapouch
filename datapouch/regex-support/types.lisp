;;;; regex-support/types.lisp


(in-package :datapouch.regex-support)


(defun list-of-regexes-p (list)
  (and (consp list)
       (every (lambda (x) (typep x 'regex))
              list)))


(deftype list-of-regexes ()
  "Is non-NIL and contains only D.REGEX:REGEX objects."
  `(satisfies list-of-regexes-p))


(deftype relaxed-regex ()
  `(or null string regex))


(defun list-of-relaxed-regexes-p (list)
  (and (consp list)
       (every (lambda (x) (or (null x)
                              (stringp x)
                              (typep x 'regex)))
              list)))


(deftype list-of-relaxed-regexes ()
  "Is non-NIL and contains only (OR D.REGEX:REGEX STRING) objects."
  `(satisfies list-of-relaxed-regexes-p))


(defun list-of-sampled-regexes-p (list)
  (and (consp list)
       (every (lambda (x) (typep x 'sampled-regex))
              list)))


(deftype list-of-sampled-regexes ()
  "Is non NIL and contains only D.REGEX:SAMPLED-REGEX objects."
  `(satisfies list-of-sampled-regexes-p))


(deftype relaxed-sampled-regex ()
  `(or null sampled-regex))


(defun list-of-relaxed-sampled-regexes-p (list)
  (and (consp list)
       (every (lambda (x) (or (null x)
                              (typep x 'sampled-regex)))
              list)))


(deftype list-of-relaxed-sampled-regexes ()
  "Is non-NIL and contains only D.REGEX:SAMPLED-REGEX objects or NILs."
  `(satisfies list-of-relaxed-sampled-regexes-p))


(defun list-of-sampled-matchers-p (list)
  (and (consp list)
       (every (lambda (x) (or (typep x 'sampled-regex)
                              (typep x 'sampled-regex-scanner)))
              list)))


(deftype list-of-sampled-matchers ()
  "Is non NIL and contains either D.REGEX:SAMPLED-REGEX or
D.REGEX:SAMPLED-REGEX-SCANNER objects."
  `(satisfies list-of-sampled-matchers-p))
