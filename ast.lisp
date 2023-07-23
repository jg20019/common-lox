(in-package #:common-lox)

(define-ast 
  expr
  (binary (expr left) (token operator) (expr right))
  (grouping (expr expression))
  (literal value)
  (unary (token operator) (expr right))
  (variable (token name)))

(define-ast 
  stmt
  (expression (expr expression))
  (print (expr expression))
  (var (token name) (expr initializer)))
