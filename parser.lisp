(in-package #:common-lox.parsing)

(defclass parser () 
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current)))

(defun parser (tokens) 
  (make-instance 'parser :tokens tokens))

(defmethod expression ((parser parser))
  (equality parser))

(defmethod equality ((parser parser))
  (let ((expr (comparison parser)))
    (while (match parser :bang-equal :equal-equal)
      (let ((operator (previous parser))
            (right (comparison parser)))
        (setf expr (common-lox::binary-expr :left expr :operator operator :right right))))
    expr))

;; Continue with creating comparison

(defmethod match ((parser parser) &rest token-types)
  (dolist (token-type token-types nil) 
    (when (check parser token-type)
      (advance parser)
      (return t))))

(defmethod check ((parser parser) token-type)
  (if (at-end-p parser) 
      nil
      (equal token-type (token-type (peek parser)))))

(defmethod advance ((parser parser))
  (unless (at-end-p parser) (incf (current parser)))
  (previous parser))

(defmethod at-end-p ((parser parser))
  (equal (peek parser) :eof))

(defmethod peek ((parser parser))
  (aref (tokens parser) (current parser)))

(defmethod previous ((parser parser))
  (aref (tokens parser) (1- (current parser))))
