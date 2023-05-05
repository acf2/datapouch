;;;; zac.asd

(defsystem zettelkasten-and-accounting
  :description "Personal application"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "datapouch-regex-support" :depends-on ("packages"))
               (:file "datapouch-cli" :depends-on ("packages" "datapouch-regex-support"))
               (:file "datapouch-sql" :depends-on ("packages"))
               (:file "datapouch-main" :depends-on ("packages" "datapouch-cli" "datapouch-sql"))
               (:file "datapouch-interaction" :depends-on ("packages" "datapouch-cli" "datapouch-main" "datapouch-regex-support")))
  :depends-on (:datapouch))
