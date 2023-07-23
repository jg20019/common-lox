(in-package #:common-lox)

(define-condition runtime-error (error) 
  ((token :initarg :token :reader token)
   (message :initarg :message :reader message)))

