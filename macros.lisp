(in-package #:common-lox)

(eval-when (:compile-toplevel) 
  (defmacro while (predicate &body body)
    `(loop while ,predicate do (progn ,@body))))
