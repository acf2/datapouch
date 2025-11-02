;;;; datapouch.asd

(defsystem datapouch
  :description "The most neat pile of info mess this side of insanity."
  :version "0.5"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "interface" :depends-on ("packages"))
               (:file "auxiliary" :depends-on ("packages"))
               (:file "regex-support" :depends-on ("packages" "interface"))
               (:file "cli" :depends-on ("packages" "interface"))

               (:file "reader-macro/main" :depends-on ("packages" "interface"))
               (:file "reader-macro/auxiliary" :depends-on ("packages" "regex-support"))

               (:file "editor" :depends-on ("packages" "interface"))
               (:file "filesystem" :depends-on ("packages" "interface"))

               (:file "sql/main" :depends-on ("packages" "auxiliary" "filesystem" "interface"))
               (:file "sql/auxiliary" :depends-on ("packages" "sql/main" "interface"))

               (:file "crypto" :depends-on ("packages" "filesystem" "interface"))
               (:file "interaction" :depends-on ("packages" "auxiliary" "cli" "regex-support" "interface"))
               (:file "expressions" :depends-on ("packages" "auxiliary" "regex-support" "reader-macro/main" "interface"))
               (:file "main" :depends-on ("packages" "cli" "expressions" "reader-macro/auxiliary" "sql/main" "editor" "filesystem" "crypto" "interface")))
  :depends-on (:cl-readline :cl-ppcre :sqlite :sxql :cl-reexport :local-time :uiop :ironclad))
