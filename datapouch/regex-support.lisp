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
           :initarg :groups
           :initform nil)))


(defun list-of-regexes-p (list)
  "Return t if LIST is non nil and contains only d.regex:regex objects."
  (and (consp list)
       (every (lambda (x) (typep x 'regex))
              list)))


(deftype list-of-regexes ()
  `(satisfies list-of-regexes-p))


(deftype relaxed-regex ()
  `(or nil string regex))


(defun list-of-relaxed-regexes-p (list)
  "Return t if LIST is non nil and contains d.regex:regex objects or strings."
  (and (consp list)
       (every (lambda (x) (or (null x)
                              (stringp x)
                              (typep x 'regex)))
              list)))


(deftype list-of-relaxed-regexes ()
  `(satisfies list-of-relaxed-regexes-p))


(defmacro regex-from-string (string)
  `(make-instance 'regex :expr ,string))


(defmethod scan ((sc regex) (target-string string) &key start end &allow-other-keys)
  (ppcre:scan (expr sc) target-string :start (or start 0) :end (or end (length target-string))))


(defgeneric wrap-in-noncapturing-group (regex))


(defmethod wrap-in-noncapturing-group ((regex regex))
  (make-instance 'regex
                 :expr (format nil "(?:~A)" (expr regex))
                 :groups (copy-list (groups regex))))


(defgeneric make-named-group (name regex)
  (:documentation "Make named matching group"))


(defmethod make-named-group ((name string) (regex regex))
  (make-instance 'regex
                 ;; NB: When one group encompasses the other, it preceedes the other in group list
                 :expr (format nil "(?<~A>~A)" name (expr regex))
                 :groups (cons name (groups regex))))


(defmethod make-named-group ((name string) (regex string))
  (make-named-group name (regex-from-string regex)))


(defmethod make-named-group ((name symbol) regex)
  (make-named-group (string name) regex))


(defgeneric concat-two (one-regex another-regex)
  (:documentation "Concatenate two regexes"))


(defmethod concat-two ((one regex) (another regex))
  (make-instance 'regex
                 :expr (format nil "~A~A" (expr one) (expr another))
                 :groups (append (groups one) (groups another))))


(defmethod concat-two ((one regex) (another string))
  (concat-two one (regex-from-string another)))


(defmethod concat-two ((one string) (another regex))
  (concat-two (regex-from-string one) another))


(defmethod concat-two ((one string) (another string))
  (concat-two (regex-from-string one)
              (regex-from-string another)))


(declaim (ftype (function (list-of-relaxed-regexes)) concat-many))
(defun concat-many (regexes)
  (reduce #'concat-two (remove nil regexes)))


(defmacro concat (&rest regexes)
  `(concat-many (list ,@regexes)))


(defgeneric combine-two (one-regex another-regex)
  (:documentation "Combine two regexes"))


(defmethod combine-two ((one regex) (another regex))
  (make-instance 'regex
                 :expr (format nil "~A|~A" (expr one) (expr another))
                 :groups (append (groups one) (groups another))))


(defmethod combine-two ((one regex) (another string))
  (combine-two one (regex-from-string another)))


(defmethod combine-two ((one string) (another regex))
  (combine-two (regex-from-string one) another))


(defmethod combine-two ((one string) (another string))
  (combine-two (regex-from-string one)
               (regex-from-string another)))


(declaim (ftype (function (list-of-relaxed-regexes)) combine-many))
(defun combine-many (regexes)
  (wrap-in-noncapturing-group (reduce #'combine-two (remove nil regexes))))


(defmacro combine (&rest regexes)
  `(combine-many (list ,@regexes)))


(declaim (ftype (function (list-of-relaxed-regexes
                            &key
                            (:separator-regex relaxed-regex)
                            (:start-regex relaxed-regex)
                            (:end-regex relaxed-regex)
                            (:null-regex relaxed-regex)))
                concat-separated))
(defun concat-separated (regexes &key
                                 ((:separator-regex sep-rx) nil)
                                 ((:start-regex start-rx) nil)
                                 ((:end-regex end-rx) nil)
                                 ((:null-regex null-rx) nil))
  "This function returns concatenation of regexes (either in form of
D.REGEX:REGEX, STRING or NIL) separated by SEPARATOR-REGEX. Separator can be
NIL. START-REGEX and END-REGEX are used respectively for the start and the end.
NULL-REGEX is used if all regexes are NIL."
  (let ((nonnil-regexes (remove nil regexes)))
    (if (null nonnil-regexes)
      null-rx
      (concat start-rx
              (concat-many
                     (cons (first nonnil-regexes)
                           (loop :for regex :in (rest nonnil-regexes)
                                 :collect sep-rx
                                 :collect regex)))
              end-rx))))


(declaim (ftype (function (list-of-relaxed-regexes
                            &key
                            (:explicit boolean)
                            (:separator-regex relaxed-regex)))
                optional-concat))
(defun optional-concat (regexes &key ((:explicit explicit) nil) ((:separator-regex sep-rx) nil))
  (let ((nonnil-regexes (remove nil regexes)))
    (when nonnil-regexes
      (combine-many
        (loop :for regex-list := nonnil-regexes :then (rest regex-list)
              :while regex-list
              :collect (if (= (length regex-list) 1)
                         (if explicit
                           (first regex-list)
                           (concat (wrap-in-noncapturing-group (first regex-list)) "?"))
                         (concat-many (loop :for regex :in regex-list
                                            :for first-regex := t :then nil
                                            :collect (if first-regex
                                                       regex
                                                       (concat sep-rx (wrap-in-noncapturing-group regex) "?"))))))))))


(defclass regex-scanner ()
  ((scanner :reader scanner
            :initform nil)
   (groups :reader groups
           :initform nil)))


(declaim (ftype (function (relaxed-regex relaxed-regex relaxed-regex)) interchange))
(defun interchange (separator one another)
  (make-instance 'regex
                 :expr (format nil "(?:~A~A~A|~2@*~A~1@*~A~@*~A)" (expr one) (expr separator) (expr another))
                 :groups (append (groups one) (groups separator) (groups another)
                                 (groups another) (groups separator) (groups one))))


(defun interchange-three (separator-regex first-regex second-regex third-regex)
  "Make regex that matches three separated regexes in any order"
  (combine (concat first-regex separator-regex (interchange separator-regex second-regex third-regex))
           (concat second-regex separator-regex (interchange separator-regex first-regex third-regex))
           (concat third-regex separator-regex (interchange separator-regex first-regex second-regex))))


(defgeneric make-scanner (regex)
  (:documentation "Make faster scanner from regex object"))


(defmethod make-scanner ((regex regex))
  (let ((new-scanner (make-instance 'regex-scanner))
        (compilation-result (multiple-value-list (ppcre:create-scanner (expr regex)))))
    (with-slots ((scanner-object scanner) (group-list groups)) new-scanner
      (setf scanner-object (first compilation-result))
      (setf group-list (second compilation-result)))
    new-scanner))


(defmethod scan ((sc regex-scanner) (target-string string) &key start end &allow-other-keys)
  (funcall (scanner sc) target-string (or start 0) (or end (length target-string))))


;;; Returns assoc-list with matches
(defun match-to-assoc (str group-list match-start match-end group-starts group-ends)
  (declare (ignore match-end))
  (when match-start
    (loop :for i :from 0 :to (1- (length group-list))
          :for group-name :in group-list
          :when (aref group-starts i)
          :collect (cons group-name (subseq str (aref group-starts i) (aref group-ends i))))))


(defmacro get-group (name groups)
  `(rest (assoc (string ,name) ,groups :test #'string=)))


(defun is-group (name groups &rest options)
  (some (lambda (option)
          (string= (get-group name groups) option))
        options))


(defun list-group-names (groups)
  (map 'list #'car groups))


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
         ;; For this, reduce must be a left fold
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


(defun match-to-group-table (str group-list match-start match-end group-starts group-ends)
  (if (= (length group-starts) 0) ; no groups in scanner
    (list (list :index 0
                :ending-groups nil
                :starting-groups nil
                :piece str))
    (let* ((groups (loop :for group :in group-list
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
                           :piece (second (assoc point piece-assoc)))))))


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


(defun match-to-group-tree (str group-list match-start match-end group-starts group-ends)
  (let ((table (match-to-group-table str group-list match-start match-end group-starts group-ends)))
    (nth-value 0 (construct-hierical-tree table nil))))
