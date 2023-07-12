;;;; package.lisp

(defpackage #:common-lox
  (:use #:cl) 
  (:export :token 
           :while))

(defpackage #:common-lox.scanning 
  (:use #:cl #:common-lox) 
  (:export :scanner
           :scan-tokens))

(defpackage #:common-lox.parsing 
  (:use #:cl #:common-lox)
  (:export :parser))
