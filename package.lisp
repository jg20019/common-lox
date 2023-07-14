;;;; package.lisp

(defpackage #:common-lox
  (:use #:cl) 
  (:export 
    
    ; toplevel
    :main 
    
    ; error functions
    :lox-error 
    
    ; expressions
    :binary-expr
    :left 
    :right
    :operator
    
    :grouping-expr
    :expression

    :literal-expr
    :value

    :unary-expr

    
    ; tokens
    :token* ; named this way to avoid conflict with generic method on runtime error
    :lexeme
    :token-type
    :literal
    :line
    
    :while))

(defpackage #:common-lox.scanning 
  (:use #:cl #:common-lox) 
  (:export :scanner
           :scan-tokens))

(defpackage #:common-lox.parsing 
  (:use #:cl #:common-lox)
  (:export :parser
           :parse))
