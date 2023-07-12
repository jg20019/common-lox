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
    (common-lox:while (match parser :bang-equal :equal-equal)
      (let ((operator (previous parser))
            (right (comparison parser)))
        (setf expr (common-lox::binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod comparison ((parser parser)) 
  (let ((expr (term parser)))
    (common-lox:while (match parser :greater :greater-equal :less :less-equal)
      (let ((operator (previous parser))
            (right (term parser)))
        (setf expr (common-lox::binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod term ((parser parser))
  (let ((expr (factor parser)))
    (common-lox:while (match parser :minus :plus)
      (let ((operator (previous parser))
            (right (factor parser)))
        (setf expr (common-lox::binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod factor ((parser parser))
  (let ((expr (unary parser)))
    (common-lox:while (match parser :star :slash)
      (let ((operator (previous parser))
            (right (unary parser)))
        (setf expr (common-lox::binary-expr :left expr :operator operator :right right))))
    expr))

(defmethod unary ((parser parser))
  (if (match parser :bang :minus)
      (let ((operator (previous parser))
            (right (unary parser)))
        (common-lox::unary-expr :operator operator :right right))
      (primary parser)))

(defmethod primary ((parser parser))
  (cond ((match parser :false) (common-lox::literal-expr :value nil)) ; nil is false in common lisp
        ((match parser :true) (common-lox::literal-expr :value t))
        ((match parser :nil) (common-lox::literal-expr :value nil))
        ((match parser :number :string) 
         (common-lox::literal-expr :value (common-lox::literal (previous parser))))
        ((match parser :left-paren) 
         (let ((expr (expression parser)))
           (consume parser :right-paren "Expect ')' after expression.")
           (common-lox::grouping-expr :expression expr)))))

(defmethod match ((parser parser) &rest token-types)
  (dolist (token-type token-types nil) 
    (when (check parser token-type)
      (advance parser)
      (return t))))

(defmethod consume ((parser parser) token-type message) 
  (if (check parser token-type)
      (advance)
      ;; TODO need to throw errors here. No idea how to do that in common lisp yet
      ;; Continue at 6.3.2
      (error ())
      )
  )

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
