(in-package #:common-lox)

(defmacro while (predicate &body body)
  `(loop while ,predicate do (progn ,@body)))
