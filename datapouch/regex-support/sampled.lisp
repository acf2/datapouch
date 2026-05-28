;;;; regex-support/sampled.lisp
;;; This package is made for much easier usage of named groups.
;;; They are not supported normally, but could be enabled for ppcre.
;;; But their handling is shit. That's why this wrapper exists.


(in-package :datapouch.regex-support)


(declaim (ftype (function ((or regex regex-scanner) list-of-strings)) regex-allows-all-samples))
(defun regex-allows-all-samples (regex samples)
  (reduce (lambda (x y) (and x y))
          (map 'list (lambda (sample)
                       (multiple-value-bind (start end) (scan regex sample)
                         (and start (= start 0) end (= end (length sample)))))
               samples)))


(declaim (ftype (function ((or regex regex-scanner) list-of-strings)) regex-denies-all-samples))
(defun regex-denies-all-samples (regex samples)
  (not (reduce (lambda (x y) (or x y))
               (map 'list (lambda (sample)
                            (multiple-value-bind (start end) (scan regex sample)
                              (and start (= start 0) end (= end (length sample)))))
                    samples))))


(defclass sampled-regex ()
  ((regex :initarg :regex
          :reader regex
          :type (or d.regex:regex d.regex:regex-scanner)) ; :name -> :name ("NAME")
   (samples :initarg :samples
            :reader samples
            :type list-of-strings))
  (:documentation "Objects of SAMPLED-REGEX class are pairs of REGEX objects
with corresponding samples, that match the regex. It can be
used to test any collection of sampled regexes for
collisions (to a certain degree)."))


(define-condition sampled-regex-error (error)
  ((reason :initarg :reason :reader reason)))


(defun make-sampled-regex (regex samples)
  (if (regex-allows-all-samples regex samples)
    (make-instance 'sampled-regex :regex regex :samples samples)
    (error 'sampled-regex-error :reason "Samples could not be matched by regex")))


; NOTE: Maybe list of regexes or regex-scanners?
(declaim (ftype (function (list-of-sampled-regexes)) find-incompatible-sampled-regexes))
(defun find-incompatible-sampled-regexes (sampled-regexes)
  (loop :for sampled-regex :in sampled-regexes
        :for incompatibilities := (loop :for target-sampled-regex :in sampled-regexes
                                        :for same := (eq sampled-regex target-sampled-regex)
                                        :for regex := (regex sampled-regex)
                                        :for samples := (samples target-sampled-regex)
                                        :when (and (not same) (not (regex-denies-all-samples regex samples)))
                                        :collect target-sampled-regex
                                        :end)
        :when incompatibilities
        :collect (cons sampled-regex incompatibilities)
        :end))


(defmethod scan ((sc sampled-regex) (target-string string) &key start end &allow-other-keys)
  (scan (regex sc) target-string :start start :end end))


;;; TODO: Rewrite it with macros to comply with DRY

(defmethod wrap-in-noncapturing-group ((sr sampled-regex))
  (make-instance 'sampled-regex
                 :regex (wrap-in-noncapturing-group (regex sr))
                 :samples (samples sr)))


(defmethod make-optional ((sr sampled-regex))
  (make-instance 'sampled-regex
                 :regex (make-optional (regex sr))
                 :samples (samples sr)))


(defmethod make-named-group ((name string) (sr sampled-regex) &optional info)
  (make-instance 'sampled-regex
                 :regex (make-named-group name (regex sr) info)
                 :samples (samples sr)))


(defmethod d.iface:concat-two ((one sampled-regex) (another sampled-regex))
  (make-instance 'sampled-regex
                 :regex (concat-two (regex one) (regex another))
                 :samples (map 'list
                               (lambda (&rest args)
                                 (apply #'concatenate 'string args))
                               (d.aux:cartesian-product (samples one) (samples another)))))


(defmethod d.iface:combine-two ((one sampled-regex) (another sampled-regex))
  (make-instance 'sampled-regex
                 :regex (combine-two (regex one) (regex another))
                 :samples (union (samples one) (samples another) :test #'string=)))
