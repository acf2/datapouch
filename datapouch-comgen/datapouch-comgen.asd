;;;; datapouch-comgen.asd

(defsystem datapouch-comgen
  :description "Command generation module for datapouch"
  :version "0.1"
  :author "Dmitry Kiselev"
  :components ((:file "packages")
               (:file "main" :depends-on ("packages")))
  :depends-on (:datapouch))
