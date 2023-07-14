;;;; common-lox.asd

(asdf:defsystem #:common-lox
  :description "Describe common-lox here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:serapeum :str)
  :components ((:file "package")
               (:file "macros")
               (:file "token")
               (:file "common-lox")
               (:file "runtime-error")
               (:file "scanner")
               (:file "expr")
               (:file "parser")
               (:file "evaluate")
               (:file "interpreter")))


(asdf:defsystem #:common-lox/bin
  :depends-on (:common-lox :with-user-abort :adopt)
  :components ((:file "main"))
  :build-operation program-op
  :build-pathname #p"~/.lisp-bin/clox"
  :entry-point "common-lox:main")
