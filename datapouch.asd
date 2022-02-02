;;;; datapouch.asd

(defsystem datapouch
  :description "The most neat pile of info mess this side of insanity."
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "datapouch-cli" :depends-on ("packages"))
               (:file "datapouch-sql" :depends-on ("packages"))
               (:file "datapouch-main" :depends-on ("packages" "datapouch-cli" "datapouch-sql"))
               (:file "datapouch-interaction" :depends-on ("packages" "datapouch-cli" "datapouch-main")))
  :depends-on (:cl-readline :sqlite :sxql :uiop))
