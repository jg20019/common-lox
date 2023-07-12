(in-package #:common-lox)

(eval-when (:compile-toplevel) 
  
(defun join-symbols (a b) 
  "Combine symbol a and b into a-b"
  (let ((a-str (string-downcase (symbol-name a)))
        (b-str (string-downcase (symbol-name b))))
    (read-from-string (concatenate 'string a-str "-" b-str))))

(defun sym-keyword (sym)
  "Convert sym to keyword 

   (sym-keyword 'hello) => :hello"
  (read-from-string (concatenate 'string ":" (string-downcase (symbol-name sym)))))

(defun create-fields (field-desc) 
  "Use field desc to generate class fields.

   It accepts a single symbol
   (create-fields 'value) (value :initarg :value :accessor value)

   Or it accepts a list of the form (type name)
   (create-fields (list 'expr 'left)) => 
     (left :initarg :left :accessor left :type expr)"
  (if (listp field-desc)
      (destructuring-bind (type name) field-desc 
        (list name 
              :initarg (sym-keyword name)
              :accessor name
              :type type))
      (list field-desc 
            :initarg (sym-keyword field-desc)
            :accessor field-desc)))

  (defun create-class-name (base-class clsname) 
    (join-symbols clsname base-class))

(defun create-ast (base-class ast)
  "Use base class and ast to generate a class"
  (let ((clsname (first ast)))
    `(defclass ,(join-symbols clsname base-class)
       (,base-class) ,(mapcar #'create-fields (rest ast)))))

(defun field-names (field-desc) 
  "Get field-name from field-desc which can be of the form
   field-name or (type field-name)"
  (if (listp field-desc) (second field-desc) field-desc))

(defun create-constructors (base-class ast)
  (let ((constructor-name (create-class-name base-class (first ast)))
        (fields (mapcar #'field-names (rest ast))))
    `(defun ,constructor-name (&key ,@fields)
       (make-instance ',constructor-name ,@(loop for field in fields appending (list (sym-keyword field) field)))))))



(defmacro define-ast (base-class &rest asts) 
  "Generate a base class and classes representing AST"
  `(progn 
     (defclass ,base-class () ())

     ,@(loop for ast in asts appending 
             (list (create-ast base-class ast)
                   (create-constructors base-class ast)))))

(define-ast 
  expr
  (binary (expr left) (token operator) (expr right))
  (grouping (expr expression))
  (literal value)
  (unary (token operator) (expr right)))

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
