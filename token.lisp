(in-package #:common-lox)

(defclass token () 
  ((token-type :initarg :type :reader token-type)
   (lexeme :initarg :lexeme :reader lexeme)
   (literal :initarg :literal :reader literal)
   (line :initarg :line :reader line)))

(defun token* (&key type lexeme (literal nil) line)
  (make-instance 'token :type type
                 :lexeme lexeme
                 :literal literal
                 :line line))
(defmethod to-string ((a-token token))
  (with-slots (token-type lexeme literal) a-token 
    (format nil "~a ~a ~a" token-type lexeme literal)))

(defmethod print-object ((a-token token) stream)
  (print-unreadable-object (a-token stream :type t) 
    (format stream (to-string a-token))))

