;;;; datapouch.asd

(defsystem datapouch
  :description "The most neat pile of info mess this side of insanity."
  :version "0.5"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))
               (:file "regex-support" :depends-on ("packages"))
               (:file "cli" :depends-on ("packages"))
               (:file "reader-macro" :depends-on ("packages" "cli" "regex-support"))
               (:file "editor" :depends-on ("packages"))
               (:file "filesystem" :depends-on ("packages"))
               (:file "sql" :depends-on ("packages" "auxiliary" "filesystem"))
               (:file "sql-auxiliary" :depends-on ("packages" "sql"))
               (:file "crypto" :depends-on ("packages" "filesystem"))
               (:file "interaction" :depends-on ("packages" "auxiliary" "cli" "regex-support"))

               (:file "shell/regex-s-form" :depends-on ("packages" "cli" "regex-support"))
               (:file "shell/expressions" :depends-on ("packages" "auxiliary" "regex-support" "shell/regex-s-form" "reader-macro"))
               (:file "shell/docs" :depends-on ("packages" "shell/expressions"))

               (:file "main" :depends-on ("packages" "cli" "shell/expressions" "sql" "editor" "filesystem" "crypto")))
  :depends-on (:cl-readline :cl-ppcre :sqlite :sxql :cl-reexport :local-time :uiop :ironclad))
