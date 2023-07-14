(in-package #:common-lox.parsing)

(defclass parser () 
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current)))

(defun parser (tokens) 
  (make-instance 'parser :tokens tokens))

(defmethod parse ((parser parser))
  (handler-case (expression parser)
    (parse-error nil)))

(defmethod expression ((parser parser))
  (equality parser))

(defmethod equality ((parser parser))
  (let ((expr (comparison parser)))
    (while (match parser :bang-equal :equal-equal)
      (let ((operator (previous parser))
            (right (comparison parser)))
        (setf expr (binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod comparison ((parser parser)) 
  (let ((expr (term parser)))
    (while (match parser :greater :greater-equal :less :less-equal)
      (let ((operator (previous parser))
            (right (term parser)))
        (setf expr (binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod term ((parser parser))
  (let ((expr (factor parser)))
    (while (match parser :minus :plus)
      (let ((operator (previous parser))
            (right (factor parser)))
        (setf expr (binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod factor ((parser parser))
  (let ((expr (unary parser)))
    (while (match parser :star :slash)
      (let ((operator (previous parser))
            (right (unary parser)))
        (setf expr (binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod unary ((parser parser))
  (if (match parser :bang :minus)
      (let ((operator (previous parser))
            (right (unary parser)))
        (unary-expr :operator operator :right right))
      (primary parser)))

(defmethod primary ((parser parser))
  (cond ((match parser :false) (literal-expr :value nil)) ; nil is false in common lisp
        ((match parser :true) (literal-expr :value t))
        ((match parser :nil) (literal-expr :value nil))
        ((match parser :number :string) 
         (literal-expr :value (literal (previous parser))))
        ((match parser :left-paren) 
         (let ((expr (expression parser)))
           (consume parser :right-paren "Expect ')' after expression.")
           (grouping-expr :expression expr)))
        (t (error (signal-error (peek parser) "Expect expression")))))

(defmethod match ((parser parser) &rest token-types)
  (dolist (token-type token-types nil) 
    (when (check parser token-type)
      (advance parser)
      (return t))))

(defmethod consume ((parser parser) token-type message) 
  (if (check parser token-type)
      (advance parser)
      (error (signal-error (peek parser) message))))

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

(defun signal-error (token message) 
  (lox-error token message)
  (make-condition 'parse-error))

(defmethod synchronize ((parser parser))
  (advance parser)
  (while (not (at-end-p parser))
    (when (equal (token-type (previous parser)) :semicolon)
      (return-from synchronize))
    (case (token-type (peek parser))
      ((:class :fun :var :for :if :while :print :return) (return-from synchronize))))
  (advance parser))
