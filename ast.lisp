(in-package #:common-lox)

(define-ast-nodes 
  expr
  (assign (token name) (expr value))
  (binary (expr left) (token operator) (expr right))
  (grouping (expr expression))
  (literal value)
  (unary (token operator) (expr right))
  (variable (token name)))

(define-ast-nodes
  stmt
  (block statements)
  (expression (expr expression))
  (print (expr expression))
  (var (token name) (expr initializer)))
