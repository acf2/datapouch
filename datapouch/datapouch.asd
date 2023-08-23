;;;; datapouch.asd

(defsystem datapouch
  :description "The most neat pile of info mess this side of insanity."
  :version "0.3"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "regex-support" :depends-on ("packages"))
               (:file "cli" :depends-on ("packages"))
               (:file "reader-macro" :depends-on ("packages" "cli" "regex-support"))
               (:file "sql" :depends-on ("packages"))
               (:file "editor" :depends-on ("packages"))
               (:file "filesystem" :depends-on ("packages"))
               (:file "main" :depends-on ("packages" "cli" "reader-macro" "sql" "editor" "filesystem"))
               (:file "interaction" :depends-on ("packages" "cli" "main" "regex-support")))
  :depends-on (:cl-readline :cl-ppcre :sqlite :sxql :cl-reexport :uiop :ironclad))
