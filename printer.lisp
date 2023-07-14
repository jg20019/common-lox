(in-package #:common-lox)

(defun parenthesize (name &rest exprs) 
  (format nil "(~a ~{~a~^ ~})" name exprs))

(defmethod print-object ((expr binary-expr) out)
  (format out (parenthesize (lexeme (operator expr))
                            (left expr)
                            (right expr))))

(defmethod print-object ((expr grouping-expr) out)
  (format out (parenthesize "group" (expression expr))))

(defmethod print-object ((expr literal-expr) out)
  (if (null (value expr))
      (format out "nil")
      (format out "~a" (value expr))))

(defmethod print-object ((expr unary-expr) out)
  (format out (parenthesize (lexeme (operator expr)) (right expr))))
