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
        ;; NB: When one group encompasses the other, it preceedes the other in group list
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


(defmacro get-group (name groups)
  `(rest (assoc (string ,name) ,groups :test #'string=)))


(defun is-group (name groups &rest options)
  (some (lambda (option)
          (string= (get-group name groups) option))
        options))


(defun list-group-names (groups)
  (map 'list #'car groups))


(defgeneric scan-to-group-table (regex string)
  (:documentation "Scan regex with named groups and make a table out of match indices"))

(defgeneric scan-to-group-tree (regex string)
  (:documentation "Scan regex with named groups to tree of matches (represented as a list)"))


; Example (for my fragile memory):
;   "aa asdf ff ss dd"
;   "aa" "asdf" "ff" "ss" "dd"
; Result:
;   "aa" (:g3 (:g2 (:g1 "asdf")) "ff") "ss" (:g1 "dd")
; Method:
;   Make (imaginary) table, group order IS IMPORTANT
;
; index | ending groups | starting groups | string that starts
; ------+---------------+-----------------+-------------------
;     0 |       -       |         -       |      "aa"
;     2 |       -       |     g3,g2,g1    |     "asdf"
;     6 |     g2,g1     |         -       |      "ff"
;     8 |      g3       |         -       |      "ss"
;    10 |       -       |        g1       |      "dd"
;    12 |      g1       |         -       |       -
;
;   Then go sequentially through the table in a function
;   Function must have a some sort of stack to remember context and accumulator for result
;   (And maybe some sort of context argument for making recursive calls in bulk)
;   Stack is S, Accumulator is A
;
; 0: No endings; no starts; string "aa"
;    Add "aa" to accumulator
;      S -> (), A: ("aa")
; 2: No endings; g3, g2, g1 start (in that order), string "asdf"
;    Call self with g3, to call - g2 g1
;      S -> (g3), A: ()
;    Call self with g2, to call - g1
;      S -> (g2 g3), A: ()
;    Call self with g1, nothing to call
;      S -> (g1 g2 g3), A: ()
;    Add "asdf" to accumulator
;      S -> (g1 g2 g3), A: ("asdf")
; 6: g2, g1 ends; no starts; string "ff"
;   [Checking ending groups in reverse order]
;     S -> (g1 g2 g3), A: ("asdf")
;   Ending call:
;     Is g1 on top of stack? Yes.
;   Return: (:group "g1" "asdf")
;     S -> (g2 g3), A: ((:group "g1" "asdf"))
;   Ending call:
;     Is g2 on top of stack? Yes.
;   Return: (:group "g2" (:group "g1" "asdf"))
;     S -> (g3), A: ((:group "g2" (:group "g1" "asdf")))
;   Add "ff" to accumulator
;     S -> (g3), A: ((:group "g2" (:group "g1" "asdf")) "ff")
; 8: g3 ends; no starts; string "ss"
;   Ending call:
;     Is g3 on top of stack? Yes.
;   Return: (:group "g3" (:group "g2" (:group "g1" "asdf")) "ff")
;     S -> (), A: ("aa" (:group "g3" (:group "g2" (:group "g1" "asdf")) "ff"))
;   Add "ss" to accumulator
;     S -> (), A: ("aa" (:group "g3" (:group "g2" (:group "g1" "asdf")) "ff") "ss")
; 10: No endings; g1 starts; string "dd"
;    Call self with g1, nothing to call
;      S -> (g1), A: ()
;   Add "dd" to accumulator
;      S -> (g1), A: ("dd")
; 12: g1 ends; no starts; no string
;   Ending call:
;     Is g1 on top of stack? Yes.
;   Return: (:group "g1" "dd")
;     S -> (), A: ("aa" (:group "g3" (:group "g2" (:group "g1" "asdf")) "ff") "ss" (:group "g1" "dd))
;
; Result: ("aa" (:group "g3" (:group "g2" (:group "g1" "asdf")) "ff") "ss" (:group "g1" "dd))


(defun group-by-numeric-parameter (value-collection parameter-collection)
  (flet ((lift (value parameter) (list (list parameter value)))
         (less (one another) (< (first (first one)) (first (first another))))
         ;; For this reduce must be a left fold
         ;; one - ((N1 ...) (N2 ...) ... (NK ...))
         ;; another - ((M1 ...))
         (join-two (one another) (if (= (first (first (last one))) (first (first another)))
                                   (append (butlast one)
                                           (list (cons (first (first (last one)))
                                                       (append
                                                         (rest (first (last one)))
                                                         (rest (first another))))))
                                   (append one another))))
    (reduce #'join-two (sort (map 'list #'lift value-collection parameter-collection)
                             #'less))))


(defmethod scan-to-group-table ((sc regex-scanner) (str string))
  (multiple-value-bind (match-start match-end group-starts group-ends) (funcall (scanner sc) str 0 (length str))
    (if (= (length group-starts) 0) ; no groups in scanner
      (list (list :index 0
                  :ending-groups nil
                  :starting-groups nil
                  :piece str))
      (let* ((groups (loop :for group :in (groups sc)
                           :for i := 0 :then (1+ i)
                           :collect (list group i)))
             (separation-points (sort (remove-duplicates (append (list match-start match-end)
                                                                 (coerce group-starts 'list)
                                                                 (coerce group-ends 'list)))
                                      #'<))
             (piece-assoc (map 'list (lambda (start end)
                                       (list start
                                             (subseq str start end)))
                               separation-points
                               (rest separation-points)))
             (groups-starting-from (group-by-numeric-parameter groups group-starts))
             (groups-ending-on (group-by-numeric-parameter groups group-ends)))
        (loop :for point :in separation-points
              :collect (list :index point
                             :ending-groups (rest (assoc point groups-ending-on))
                             :starting-groups (rest (assoc point groups-starting-from))
                             :piece (second (assoc point piece-assoc))))))))


(defun construct-hierical-tree (table group-stack)
  (loop
    :with iteration-result
    :with context := table
    :for current-context := (first context)
    :for starting-position := (member (first group-stack)
                                            (getf current-context :starting-groups)
                                            :test #'equal)
    :for starting-list := (or (rest starting-position)
                              (and (null starting-position)
                                   (getf current-context :starting-groups)))
    ;; null ending: unique for top call (with empty group-stack))
    :if (null context)
    :return (values resulting-tree nil)
    :end
    ;; checking if groups should be closed (and their result returned)
    :when (member (first group-stack) (getf current-context :ending-groups) :test #'equal)
    :return (values (list* :group (first (first group-stack)) resulting-tree)
                    context)
    :end
    ;; open group (and collect result of the call)
    :if starting-list
    :do (multiple-value-setq (iteration-result context)
          (construct-hierical-tree context (cons (first starting-list)
                                                 group-stack)))
    :else
    ;; collect term
    :do (setf iteration-result (getf current-context :piece)
              context (rest context))
    :end
    :when iteration-result
    :collect iteration-result :into resulting-tree
    :end))


(defmethod scan-to-group-tree ((sc regex-scanner) (str string))
  (let ((table (scan-to-group-table sc str)))
    (nth-value 0 (construct-hierical-tree table nil))))
