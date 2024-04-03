;;;; datapouch.asd

(defsystem datapouch
  :description "The most neat pile of info mess this side of insanity."
  :version "0.3"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "auxiliary" :depends-on ("packages"))
               (:file "regex-support" :depends-on ("packages"))
               (:file "cli" :depends-on ("packages"))
               (:file "reader-macro" :depends-on ("packages" "cli" "regex-support"))
               (:file "editor" :depends-on ("packages"))
               (:file "filesystem" :depends-on ("packages"))
               (:file "sql" :depends-on ("packages" "auxiliary" "filesystem"))
               (:file "crypto" :depends-on ("packages" "filesystem"))
               (:file "interaction" :depends-on ("packages" "auxiliary" "cli" "regex-support"))
               (:file "main" :depends-on ("packages" "cli" "reader-macro" "sql" "editor" "filesystem" "crypto")))
  :depends-on (:cl-readline :cl-ppcre :sqlite :sxql :cl-reexport :local-time :uiop :ironclad))
