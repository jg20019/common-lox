(in-package #:common-lox)

(defclass interpreter () ())

(defmethod interpret ((interpreter interpreter) statements)
  (handler-case 
    (dolist (statement statements) 
      (evaluate statement))
    (runtime-error (e) 
                   (runtime-error e))))

(defun interpreter () 
  (make-instance 'interpreter))

(defmethod evaluate ((stmt expression-stmt))
  (evaluate (expression stmt)))

(defmethod evaluate ((stmt print-stmt))
  (let ((value (evaluate (expression stmt))))
    (format t "~a~%" (stringify value))))

(defmethod evaluate ((expr literal-expr)) 
  (value expr))

(defmethod evaluate ((expr grouping-expr))
  (evaluate (expression  expr)))

(defmethod evaluate ((expr unary-expr))
  (let ((right (evaluate (right expr))))
    (case (token-type (operator expr))
      (:minus (progn (check-number-operand (operator expr) right) (- right)))
      (:bang (not right)))))

(defmethod evaluate ((expr binary-expr))
  (let ((left (evaluate (left expr)))
        (right (evaluate (right expr)))
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
