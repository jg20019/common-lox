;;;; package.lisp

(defpackage #:common-lox
  (:use #:cl) 
  (:export 
    
    ; toplevel
    :main 
    
    ; error functions
    :lox-error 
    
    ; expressions
    :assign-expr
    :token
    :expr 
    
    :binary-expr
    :left 
    :right
    :operator

    :call-expr
    :callee
    :paren
    :arguments
    
    :grouping-expr
    :expression

    :literal-expr
    :value

    :logical-expr

    :unary-expr
    :variable-expr

    ; statements
    :print-stmt
    :expression-stmt
    :if-stmt
    :if-condition
    :then-branch
    :else-branch
    :var-stmt
    :name
    :initializer
    :block-stmt
    :statements
    :while-stmt
    :while-condition
    :body
    
    ; tokens
    :token* ; named this way to avoid conflict with generic method on runtime error
    :lexeme
    :token-type
    :literal
    :line

    ; environment
    :new-environment
    :enclosing
    :env-values
    
    :while))

(defpackage #:common-lox.scanning 
  (:use #:cl #:common-lox) 
  (:export :scanner
           :scan-tokens))

(defpackage #:common-lox.parsing 
  (:use #:cl #:common-lox)
  (:export :parser
           :parse))
