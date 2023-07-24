(in-package #:common-lox)

(defclass interpreter ()
  ((environment :initform (make-instance 'environment) 
                :initarg :environment
                :accessor environment)))

(defmethod interpret ((interpreter interpreter) statements)
  (handler-case 
    (dolist (statement statements) 
      (evaluate interpreter statement))
    (runtime-error (e) 
      (runtime-error e))))

(defun interpreter () 
  (make-instance 'interpreter))

(defmethod evaluate ((interpreter interpreter) (stmt expression-stmt))
  (evaluate interpreter (expression stmt)))

(defmethod evaluate ((interpreter interpreter) (stmt print-stmt))
  (let ((value (evaluate interpreter (expression stmt))))
    (format t "~a~%" (stringify value))))

(defmethod evaluate ((interpreter interpreter) (stmt var-stmt))
  (let (value)
    (when (initializer stmt)
      (setf value (evaluate interpreter (initializer stmt))))
    (define (environment interpreter) (lexeme (name stmt)) value)
    nil))

(defmethod evaluate ((interpreter interpreter) (expr assign-expr))
  (let ((value (evaluate interpreter (value expr))))
    (assign (environment interpreter) (name expr) value)
    value))

(defmethod evaluate ((interpreter interpreter) (expr literal-expr)) 
  (value expr))

(defmethod evaluate ((interpreter interpreter) (expr grouping-expr))
  (evaluate interpreter (expression  expr)))

(defmethod evaluate ((interpreter interpreter) (expr unary-expr))
  (let ((right (evaluate (right expr))))
    (case (token-type (operator expr))
      (:minus (progn (check-number-operand (operator expr) right) (- right)))
      (:bang (not right)))))

(defmethod evaluate ((interpreter interpreter) (expr variable-expr))
  (getValue (environment interpreter) (name expr)))

(defmethod evaluate ((interpreter interpreter) (expr binary-expr))
  (let ((left (evaluate interpreter (left expr)))
        (right (evaluate interpreter (right expr)))
        (operator (operator expr)))
    (case (token-type operator)
      (:minus 
        (progn
          (check-number-operands operator left right)
          (- left right)))
      (:slash 
        (progn 
          (check-number-operands operator left right)
          (/ left right)))
      (:star  
        (progn 
          (check-number-operands operator left right)
          (* left right)))
      (:plus 
        (cond ((and (numberp left) (numberp right)) (+ left right))
              ((and (stringp left) (stringp right)) (concatenate 'string left right))
              (t (error 'runtime-error :token operator :message "Operands must be two numbers or two strings."))))
      (:greater 
        (progn 
          (check-number-operands operator left right)
          (> left right)))
      (:greater-equal 
        (progn 
          (check-number-operands operator left right)
          (>= left right)))
      (:less 
        (progn 
          (check-number-operands operator left right)
          (< left right)))
      (:less-equal (progn 
                     (check-number-operands operator left right)
                     (<= left right)))
      (:bang-equal (not (equal left right)))
      (:equal-equal (equal left right)))))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (error 'runtime-error :token operator :message "Operand must be a number")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (error 'runtime-error :token operator :message "Operands must be numbers.")))

(defun stringify (object) 
  (cond ((null object) "nil")
        ((numberp object) (let ((text (format nil "~a" object)))
                            (if (str:ends-with-p ".0" text)
                               (subseq text 0 (- (length text) 2))
                               text)))
        (t (format nil "~a" object))))
