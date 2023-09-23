;;;; crypto.lisp


(in-package :d.crypto)


(defun sha512-for-file (path)
  (with-open-file (file path
                        :direction :input
                        :element-type '(unsigned-byte 8))
    (ironclad:digest-stream 'ironclad:sha512 file)))


(defun write-checksum-to-file (path checksum)
  (with-open-file (file path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~(~{~16,2,'0R~}~)~%" (coerce checksum 'list))))


(defun read-checksum-from-file (path)
  (with-open-file (file path
                        :direction :input)
    (let ((byte-array-string (string-trim '(#\Newline #\Space #\Tab) (read-line file))))
      (coerce (loop for i from 0 to (1- (length byte-array-string)) by 2
                    collect (parse-integer byte-array-string
                                           :start i
                                           :end (+ i 2)
                                           :radix 16))
              '(vector (unsigned-byte 8))))))


(defparameter *control-database-integrity* t)
(defparameter *advise-full-sqlite-integrity-check* nil)


(defun check-database-integrity ()
  (when *control-database-integrity*
    (let ((checksum-path (merge-pathnames +checksum-extension+ *database-path*)))
      (cond ((probe-file checksum-path)
             (unless (equalp (sha512-for-file *database-path*)
                             (read-checksum-from-file checksum-path))
               (setf *backup-tiers* nil)
               (setf *advise-full-sqlite-integrity-check* t)
               (setf *control-database-integrity* nil)
               (unless (yes-or-no-p "Attention! Database hashes do not match. Backup functionality is disabled.~&To fix the problem, load earlier backup, or remove checksum file (at your own risk)~&Do you want to load the database?")
                 (setf *database-path* nil))))
            (*database-path*
              (setf *advise-full-sqlite-integrity-check* t)
              (unless (yes-or-no-p "There is no checksum for the database.~&Are you sure you want to load it?")
                (setf *database-path* nil)
                (setf *backup-tiers* nil)))))))


(defun rehash-database ()
  (when *control-database-integrity*
    (let ((checksum-path (merge-pathnames +checksum-extension+ *database-path*)))
      (write-checksum-to-file checksum-path (sha512-for-file *database-path*)))))
