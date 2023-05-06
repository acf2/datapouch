;;;; bookkeeping.lisp


(in-package :zac.book)


(defparameter +regex-minute+ (make-named-group "minute" (make-instance 'regex :expr "\\d{2}")))
(defparameter +regex-short-hour+ (make-named-group "hour" (make-instance 'regex :expr "\\d{1,2}")))
(defparameter +regex-long-hour+ (make-named-group "hour" (make-instance 'regex :expr "\\d{2}")))
(defparameter +regex-time+ (combine (concat +regex-short-hour+ ":" +regex-minute+)
                                    (concat +regex-long-hour+ +regex-minute+)))


(defparameter +regex-short-day+ (make-named-group "day" (make-instance 'regex :expr "\\d{1,2}")))
(defparameter +regex-long-day+ (make-named-group "day" (make-instance 'regex :expr "\\d{2}")))
(defparameter +regex-short-month+ (make-named-group "month" (make-instance 'regex :expr "\\d{1,2}")))
(defparameter +regex-long-month+ (make-named-group "month" (make-instance 'regex :expr "\\d{2}")))
(defparameter +regex-rus-str-month+ (make-named-group "month" (make-instance 'regex :expr "[а-яА-Я]+"))) ; ???
(defparameter +regex-short-year+ (make-named-group "year" (combine "\\d{2}" "\\d{4}")))
(defparameter +regex-long-year+ (make-named-group "year" (make-instance 'regex :expr "\\d{4}")))
(defparameter +regex-date+ (combine (concat +regex-long-year+ "-" +regex-long-month+ "-" +regex-long-day+)
                                    (concat +regex-short-day+ "\\." +regex-short-month+ "\\." +regex-short-year+)
                                    (concat +regex-short-day+ "\\s+" +regex-rus-str-month+ "\\s+" +regex-long-year+)))


(defparameter +regex-separator+ (combine ",\\s*" "\\s+"))
(defparameter +regex-datetime+ (interchange +regex-separator+ +regex-time+ +regex-date+))
