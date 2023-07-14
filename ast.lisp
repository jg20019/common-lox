(in-package #:common-lox)

(define-ast 
  expr
  (binary (expr left) (token operator) (expr right))
  (grouping (expr expression))
  (literal value)
  (unary (token operator) (expr right)))
