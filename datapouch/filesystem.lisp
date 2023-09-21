;;;; filesystem.lisp


(in-package :datapouch.filesystem)


(defun ensure-file-exists (path &optional (initial-text nil))
  (or (probe-file path)
      (progn
        (ensure-directories-exist (make-pathname :directory (pathname-directory path)))
        (with-open-file (file path :direction :output)
          (when initial-text
            (write initial-text :stream file))))))


(defun timestamp-difference (one-timestamp another-timestamp)
  (- (local-time:timestamp-to-universal one-timestamp)
     (local-time:timestamp-to-universal another-timestamp)))


(defun max-timestamp (one-timestamp &rest other-timestamps)
  (if other-timestamps
    (reduce (lambda (one another)
              (if (local-time:timestamp> one another)
                one
                another))
            (cons one-timestamp other-timestamps))
    one-timestamp))


(defun format-timestamp-1ttf (timestamp)
  (local-time:format-timestamp nil timestamp :format '(:year "-" (:month 2) "-" (:day 2))))


(defclass database-file ()
  ((path :initarg :path
         :reader path)
   (checksum-path :initarg :checksum-path
                  :reader checksum)
   (date :initarg :date
         :reader date)))


(defmethod initialize-instance :around ((file database-file) &rest rest &key path &allow-other-keys)
  (when (probe-file path)
    (let ((checksum-path (merge-pathnames +checksum-extension+ path))
          (date (local-time:parse-timestring (pathname-name path))))
      (apply #'call-next-method file (append (list :checksum-path (and (probe-file checksum-path) checksum-path)
                                                   :date date)
                                             rest)))))


(defun discover-all-databases-in-directory (path-to-databases)
  ;; Probably not the best decision, but it works
  (when (and (probe-file path-to-databases)
             (not (pathname-name (probe-file path-to-databases)))) ; directory, not a file
      (loop for path in (directory (reduce #'merge-pathnames (list +database-extension+
                                                                   (make-pathname :name :wild)
                                                                   path-to-databases)))
            collect (make-instance 'database-file :path path))))


;;; Higher level

;; .config/datapouch works too, but is not accurate (there is more that just configs)
(defparameter +application-folder+ (merge-pathnames #P".datapouch/" (user-homedir-pathname)))
(defparameter +working-directory+ (directory-namestring (or *load-truename* *default-pathname-defaults*)))


(defparameter +database-extension+ (make-pathname :type "db"))
(defparameter +checksum-extension+ (make-pathname :type "chk"))


(defparameter *database-path* (reduce #'merge-pathnames (list +database-extension+ "current" +application-folder+)))
(defparameter *history-path* (merge-pathnames #P"history" +application-folder+))


(defparameter *backup-tiers* `((:daily :days 1
                                       :directory (merge-pathnames #P"backups/daily/" +application-folder+)
                                       :rotation 10)
                               (:weekly :days 7
                                        :directory (merge-pathnames #P"backups/weekly/" +application-folder+)
                                        :rotation 10)
                               (:monthly :days 28
                                         :directory (merge-pathnames #P"backups/monthly/" +application-folder+)
                                         :rotation nil)))


(defun register-backups (backup-tiers)
  (mapcar
    (lambda (backup-tier)
      (append backup-tier (list :databases (discover-all-databases-in-directory (getf (rest backup-tier) :directory)))))
    backup-tiers))


(defun filter-backups-due (timestamp registered-backups)
  (let ((seconds-in-day (* 24 60 60)))
    (remove-if-not (lambda (backup-tier)
                     (>= (timestamp-difference timestamp
                                               (reduce #'max-timestamp
                                                       (mapcar #'date (getf backup-tier :databases))))
                         (* (getf backup-tier :days) seconds-in-day)))
                   registered-backups)))


(defun ensure-backup-system-is-inited ()
  (loop for backup-tier in *backup-tiers*
        do (ensure-directories-exist (getf backup-tier :directory))))


;;; App files initialization
(defun application-files-init ()
  (ensure-file-exists *database-path*)
  (when *history-path* (ensure-file-exists *history-path*))
  (when *backup-tiers* (ensure-backup-system-is-inited)))


(defun remove-obsolete-backups (databases amount-to-keep)
  (loop for obsolete-backup in (nthcdr amount-to-keep
                                       (sort databases
                                             #'local-time:timestamp>
                                             :key #'date))
        do
        (delete-file (path obsolete-backup))
        (when (checksum-path obsolete-backup)
          (delete-file (checksum-path obsolete-backup)))))


(defun make-backup (backup-directory new-backup-filename)
  (uiop:copy-file *database-path*
                  (merge-pathnames new-backup-filename backup-directory))
  (uiop:copy-file (make-pathname :type +checksum-extension+
                                 :defaults *database-path*)
                  (merge-pathnames (make-pathname :type +checksum-extension+
                                                  :defaults new-backup-filename)
                                   backup-directory)))


(defun process-all-backup-tiers ()
  (let* ((backups (register-backups *backup-tiers*))
         (now (local-time:now))
         (new-backup-filename (make-pathname :name (format-timestamp-1ttf now)
                                             :type +database-extension+)))
    (loop for backup-tier in (filter-backups-due now backups)
          do
          (when (getf backup-tier :rotation)
            (remove-obsolete-backups (getf backup-tier :databases)
                                     (1- (getf backup-tier :rotation))))
          (make-backup (getf backup-tier :directory)
                       new-backup-filename))))
