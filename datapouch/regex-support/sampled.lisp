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


(declaim (ftype (function (string))
                make-anycase-samples))
(defun make-anycase-samples (string)
  (d.aux:cartesian-product
    (map 'list (lambda (chr)
                 (let ((uchr (char-upcase chr)))
                   (if (char= chr uchr)
                     (list chr)
                     (list chr uchr))))
         (coerce (string-downcase string) 'list))
    (lambda (&rest var)
      (coerce var 'string))))


(defclass sampled-regex (regex)
  ((samples :initarg :samples
            :reader samples
            :type list-of-strings))
  (:documentation "Objects of SAMPLED-REGEX class are pairs of REGEX objects
with corresponding samples, that match the regex. It can be
used to test any collection of sampled regexes for
collisions (to a certain degree)."))


(define-condition sampled-regex-error (error)
  ((reason :initarg :reason :reader reason)))


(declaim (ftype (function (string list-of-strings)) sampled-regex-from-string))
(defun sampled-regex-from-string (string samples)
  "Make D.REGEX:SAMPLED-REGEX instance from a STRING, that contains a regex,
and a list of samples."
  (let ((sr (make-instance 'sampled-regex
                           :tree (ppcre:parse-string string)
                           :samples samples)))
    (if (regex-allows-all-samples sr samples)
      sr
      (error 'sampled-regex-error :reason "Regex does not allow all samples"))))


; NOTE: Maybe list of regexes or regex-scanners?
(declaim (ftype (function (list-of-sampled-matchers)) find-incompatible-sampled-regexes))
(defun find-incompatible-sampled-regexes (sampled-regexes)
  (loop :for sampled-regex :in sampled-regexes
        :for incompatibilities := (loop :for target-sampled-regex :in sampled-regexes
                                        :for same := (eq sampled-regex target-sampled-regex)
                                        :for regex := sampled-regex
                                        :for samples := (samples target-sampled-regex)
                                        :when (and (not same) (not (regex-denies-all-samples regex samples)))
                                        :collect target-sampled-regex
                                        :end)
        :when incompatibilities
        :collect (cons sampled-regex incompatibilities)
        :end))


;;; TODO: Rewrite it with macros to comply with DRY

(defmethod make-optional :around ((sr sampled-regex))
  (let ((result (call-next-method)))
    (make-instance 'sampled-regex
                   :tree (tree result)
                   :group-map (group-map result)
                   :samples (cons "" (samples sr)))))

(defmethod make-named-group :around (name (sr sampled-regex) &optional info)
  (declare (ignore info))
  (let ((result (call-next-method)))
    (make-instance 'sampled-regex
                   :tree (tree result)
                   :group-map (group-map result)
                   :samples (samples sr))))


(defmethod d.iface:concat-two :around ((one sampled-regex) (another sampled-regex))
  (let ((result (call-next-method)))
    (make-instance 'sampled-regex
                   :tree (tree result)
                   :group-map (group-map result)
                   :samples (map 'list
                                 (lambda (joined-samples)
                                   (apply #'concatenate 'string joined-samples))
                                 (d.aux:cartesian-product (list (samples one) (samples another)))))))


(defmethod d.iface:combine-two ((one sampled-regex) (another sampled-regex))
  (let ((result (call-next-method)))
    (make-instance 'sampled-regex
                   :tree (tree result)
                   :group-map (group-map result)
                   :samples (union (samples one) (samples another) :test #'string=))))


(defclass sampled-regex-scanner (regex-scanner)
  ((samples :initarg :samples
            :reader samples
            :type list-of-strings))
  (:documentation ""))


(defmethod make-scanner :around ((sr sampled-regex))
  (let ((sc (call-next-method)))
    (make-instance 'sampled-regex-scanner
                   :scanner (scanner sc)
                   :group-map (group-map sc)
                   :group-list (group-list sc)
                   :samples (samples sr))))
