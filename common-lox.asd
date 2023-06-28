;;;; common-lox.asd

(asdf:defsystem #:common-lox
  :description "Describe common-lox here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "common-lox")
               (:file "token")
               (:file "scanner")))
