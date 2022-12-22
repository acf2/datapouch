;;;; datapouch-regex-support.lisp
;;; This package is made for much easier usage of named groups.
;;; They are not supported normally, but could be enabled for ppcre.
;;; But their handling is shit. That's why this wrapper exists.

(in-package :datapouch.regex-support)

(setq ppcre:*allow-named-registers* t)

(defclass regex ()
  ((expr :reader expr
         :initarg :expr)
   (groups :reader groups
           :initform nil)))

(defgeneric wrap-in-noncapturing-group (regex))

(defmethod wrap-in-noncapturing-group ((regex regex))
  (let ((new-regex (make-instance 'regex)))
    (with-slots (expr groups) regex
      (with-slots ((new-expr expr) (new-groups groups)) new-regex
        (setf new-groups (copy-list groups))
        (setf new-expr (format nil "(?:~A)" expr))))
    new-regex))

(defgeneric make-named-group (name regex)
  (:documentation "Make named matching group"))

(defmethod make-named-group ((name string) (regex regex))
  (let ((new-regex (make-instance 'regex)))
    (with-slots (expr groups) regex
      (with-slots ((new-expr expr) (new-groups groups)) new-regex
        (setf new-expr (format nil "(?<~A>~A)" name expr))
        (setf new-groups (cons name groups))))
    new-regex))

(defgeneric concat-two (one-regex another-regex)
  (:documentation "Concatenate two regexes"))

(defmethod concat-two ((one regex) (another regex))
  (let ((new-regex (make-instance 'regex)))
    (with-slots ((one-expr expr) (one-groups groups)) one
      (with-slots ((another-expr expr) (another-groups groups)) another
        (with-slots ((new-expr expr) (new-groups groups)) new-regex
          (setf new-expr (format nil "~A~A" one-expr another-expr))
          (setf new-groups (append one-groups another-groups)))))
    new-regex))

(defmethod concat-two ((one regex) (another string))
  (let ((new-regex (make-instance 'regex)))
    (with-slots ((one-expr expr) (one-groups groups)) one
      (with-slots ((new-expr expr) (new-groups groups)) new-regex
        (setf new-expr (format nil "~A~A" one-expr another))
        (setf new-groups (copy-list one-groups))))
    new-regex))

(defmethod concat-two ((one string) (another regex))
  (let ((new-regex (make-instance 'regex)))
    (with-slots ((another-expr expr) (another-groups groups)) another
      (with-slots ((new-expr expr) (new-groups groups)) new-regex
        (setf new-expr (format nil "~A~A" one another-expr))
        (setf new-groups (copy-list another-groups))))
    new-regex))

(defun concat (&rest regexes)
  (reduce (lambda (x y) (concat-two x y)) regexes))

(defun combine (&rest regexes)
  (flet ((combine-two (one-regex another-regex)
           (let ((new-regex (make-instance 'regex)))
             (with-slots ((one-expr expr) (one-groups groups)) one-regex
               (with-slots ((another-expr expr) (another-groups groups)) another-regex
                 (with-slots ((new-expr expr) (new-groups groups)) new-regex
                   (setf new-expr (format nil "~A|~A" one-expr another-expr))
                   (setf new-groups (append one-groups another-groups)))))
             new-regex)))
    (wrap-in-noncapturing-group (reduce #'combine-two (map 'list (lambda (wannabe-regex)
                                                                   (if (typep wannabe-regex 'regex)
                                                                     wannabe-regex
                                                                     (make-instance 'regex :expr wannabe-regex)))
                                                           regexes)))))

(defgeneric interchange (separator-regex one-regex another-regex)
  (:documentation "Make regex that matches two separated regexes in any order"))

(defmethod interchange ((separator regex) (one regex) (another regex))
  (let ((new-regex (make-instance 'regex)))
    (with-slots ((sep-expr expr) (sep-groups groups)) separator
      (with-slots ((one-expr expr) (one-groups groups)) one
        (with-slots ((another-expr expr) (another-groups groups)) another
          (with-slots ((new-expr expr) (new-groups groups)) new-regex
            (setf new-expr (format nil "(?:~A~A~A|~2@*~A~1@*~A~@*~A)" one-expr sep-expr another-expr))
            (setf new-groups (append one-groups sep-groups another-groups
                                     another-groups sep-groups one-groups))))))
    new-regex))

;;; TODO Rewrite this simpler with (interchange sep first (interchange sep second third))
(defun interchange-three (separator-regex first-regex second-regex third-regex)
  "Make regex that matches three separated regexes in any order"
  (combine (concat first-regex separator-regex (interchange separator-regex second-regex third-regex))
           (concat second-regex separator-regex (interchange separator-regex first-regex third-regex))
           (concat third-regex separator-regex (interchange separator-regex first-regex second-regex))))

(defgeneric scan-named-groups (regex str)
  (:documentation "Find named groups in string"))

;;; NOTE something regarding (ppcre:scan-to-strings) ?

(defmethod scan-named-groups ((regex regex) (str string))
  (remove-if #'null
             (map 'list
                  (lambda (match key)
                    (and match (cons key match)))
                  (nth-value 1 (ppcre:scan-to-strings (expr regex) str))
                  (groups regex))))
