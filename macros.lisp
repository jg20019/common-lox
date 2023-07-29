(in-package #:common-lox)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  
(defun join-symbols (a b) 
  "Combine symbol a and b into a-b"
  (let ((a-str (string-downcase (symbol-name a)))
        (b-str (string-downcase (symbol-name b))))
    (read-from-string (concatenate 'string a-str "-" b-str))))

(defun sym-keyword (sym)
  "Convert sym to keyword. 
   
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

  (defun create-class (clsname base-class ast)
    "Create class based on ast"
    `(defclass ,clsname
       (,base-class) ,(mapcar #'create-fields (rest ast))))

(defun field-name (field-desc) 
  "Get field name from field-desc which can be of the form
   field-name or (type field-name)"
  (if (listp field-desc) (second field-desc) field-desc))

  (defun create-constructor (cls-name field-names)
    "Create a constructor function that matches cls-name. 
     field-names become keyword parameters."
    `(defun ,cls-name (&key ,@field-names)
       (make-instance ',cls-name ,@(loop for field in field-names appending (list (sym-keyword field) field))))))


(defmacro define-ast-nodes (base-class &rest subclass-specifiers) 
  "This toplevel macro defines an abstract class named BASE-CLASS and a collection of 
   concrete subclasses to form a disjoint union of types. This is similar to defining an 
   algebraic data type.

   In addition to defining classes, simple constructor functions are defined as well, whcih share
   the same name of the classes.
   
   The syntax is 
       (define-ast-nodes <base-class> <subclass-specifier>*)
   where <subclass-specifier> is a list 
       (<name> <field-name>*) 
   so that the subclass <name>-<base-class> will contain slots named <field-name>. 
   <field-name> can be a symbol representing the name of the field or a list 
       (<type> <name>)
   specifying that a field has a given type. 
   Whether this is enforced is implementation dependent.

   Functions that look like
       (defun <name>-<base-class> (&key <field-name>...) ...)
   will be created which can be used to construct the values of each respective subclass.

   An example for a set of classes representing expressions might be:

   (define-ast-nodes 
     expr
     (binary (expr left) (token operator) (expr right)
     (literal value))

   Will create classes EXPR, BINARY-EXPR, and LITERAL-EXPR. 
   It will create a function #'BINARY-EXPR which takes keyword arguments left, operator, and right 
   and a function #'LITERAL-EXPR which takes a keyword argument value.
   "
  `(progn 
     (defclass ,base-class () ())

     ,@(loop for spec in subclass-specifiers appending 
             (let ((clsname (join-symbols (first spec) base-class)))
               (list (create-class clsname base-class spec)
                     (create-constructor clsname (mapcar #'field-name (rest spec))))))) )

(defmacro while (predicate &body body)
  `(loop while ,predicate do (progn ,@body)))
