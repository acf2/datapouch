;;;; regex-support.lisp
;;; This package is made for much easier usage of named groups.
;;; They are not supported normally, but could be enabled for ppcre.
;;; But their handling is shit. That's why this wrapper exists.


(in-package :datapouch.regex-support)


;;; Allow named registers and make control abstraction
(setf ppcre:*allow-named-registers* t)


(defun allow-named-registers (&optional (flag t))
  (setf ppcre:*allow-named-registers* flag))


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


(defmethod make-named-group ((name string) (regex string))
  (let ((new-regex (make-instance 'regex)))
      (with-slots ((new-expr expr) (new-groups groups)) new-regex
        (setf new-expr (format nil "(?<~A>~A)" name regex))
        (setf new-groups (list name)))
    new-regex))


(defmethod make-named-group ((name symbol) regex)
  (make-named-group (string name) regex))


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


(defmethod concat-two ((one string) (another string))
  (let ((new-regex (make-instance 'regex)))
    (with-slots ((new-expr expr) (new-groups groups)) new-regex
      (setf new-expr (format nil "~A~A" one another)))
    new-regex))


(defun concat (&rest regexes)
  (reduce (lambda (x y)
            (cond ((null x) y)
                  ((null y) x)
                  (:else (concat-two x y))))
          regexes))


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


(defun interchange-three (separator-regex first-regex second-regex third-regex)
  "Make regex that matches three separated regexes in any order"
  (combine (concat first-regex separator-regex (interchange separator-regex second-regex third-regex))
           (concat second-regex separator-regex (interchange separator-regex first-regex third-regex))
           (concat third-regex separator-regex (interchange separator-regex first-regex second-regex))))


(defgeneric scan-named-groups (regex str)
  (:documentation "Find named groups in string"))


;;; Returns T if string matches, nil if not
;;; Returns second value - assoc-list with matches
(defmethod scan-named-groups ((regex regex) (str string))
  (let* ((scan-result (multiple-value-list (ppcre:scan-to-strings (expr regex) str)))
         (match (first scan-result))
         (groups (and match (second scan-result))))
    (if match
      (values t
              (remove-if #'null
                         (map 'list
                              (lambda (match key)
                                (and match (cons key match)))
                              groups
                              (groups regex))))
      (values nil nil))))


(defclass regex-scanner ()
  ((scanner :reader scanner
            :initform nil)
   (groups :reader groups
           :initform nil)))


(defgeneric make-scanner (regex)
  (:documentation "Make faster scanner from regex object"))


(defmethod make-scanner ((regex regex))
  (let ((new-scanner (make-instance 'regex-scanner))
        (compilation-result (multiple-value-list (ppcre:create-scanner (expr regex)))))
    (with-slots ((scanner-object scanner) (group-list groups)) new-scanner
      (setf scanner-object (first compilation-result))
      (setf group-list (second compilation-result)))
    new-scanner))


;;; Returns T if string matches, nil if not
;;; Returns second value - assoc-list with matches
(defmethod scan-named-groups ((sc regex-scanner) (str string))
  (multiple-value-bind (match-start match-end group-starts group-ends) (funcall (scanner sc) str 0 (length str))
    (declare (ignore match-end))
    (if match-start
      (values t (loop :for i :from 0 :to (1- (length (groups sc)))
                      :for group-name :in (groups sc)
                      :when (aref group-starts i)
                      :collect (cons group-name (subseq str (aref group-starts i) (aref group-ends i)))))
      (values nil nil))))


(defun get-group (name groups)
  (rest (assoc (string name) groups :test #'string=)))


; command-regex    = regex
; argument-regexes = nil | (argument-regex...)
; argument-regex   = regex | (regex modifier...)
; modifier = :optional
;  Note: Only optional is implemented for now
(defun make-command-regex-scanner (command-regex &rest argument-regexes)
  (make-scanner (apply #'concat (append (list "\\s*"
                                              command-regex)
                                        (mapcar (lambda (argument-regex)
                                                  (if (listp argument-regex)
                                                    (let ((result (concat-two "\\s+" (first argument-regex)))
                                                          (modifiers (rest argument-regex)))
                                                      (cond ((member :optional modifiers)
                                                             (setf result (concat (wrap-in-noncapturing-group result) "?")))))
                                                    (concat-two "\\s+" argument-regex)))
                                                argument-regexes)
                                        (list "\\s*")))))
