;;;; packages.lisp


(in-package :cl-user)


(defpackage :common.auxiliary
  (:use #:cl)
  (:export #:expt-mod))


(defpackage :common.patterns
  (:use #:cl)
  (:import-from :datapouch
                #:pattern))


;;; Parent package, using cl-reexport
;;; (No, dun want asdf3 and bla-bla-bla. Muh luddite faith doesn't allow it.)
(defpackage :common
  (:use #:cl))


(in-package :common)
(cl-reexport:reexport-from :common.auxiliary)
(cl-reexport:reexport-from :common.patterns)
(in-package :cl-user)
