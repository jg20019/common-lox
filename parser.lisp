(in-package #:common-lox.parsing)

(defclass parser () 
  ((tokens :initarg :tokens :reader tokens)
   (current :initform 0 :accessor current)))

(defun parser (tokens) 
  (make-instance 'parser :tokens tokens))

(defmethod parse ((parser parser))
  (handler-case 
    (let (statements) 
      (while (not (at-end-p parser))
        (push (parse-declaration parser) statements))
      (nreverse statements))
    (parse-error nil)))

(defmethod expression ((parser parser))
  (assignment parser))

(defmethod parse-declaration ((parser parser))
  (handler-case 
    (if (match parser :var) 
        (var-declaration parser)
        (statement parser))
    (parse-error ()
      (synchronize parser)
      nil)))

(defmethod statement ((parser parser))
  (cond ((match parser :print) (print-statement parser))
        ((match parser :left-brace) (block-stmt :statements (parse-block parser)))
        ((match parser :for) (for-statement parser))
        ((match parser :if) (if-statement parser))
        ((match parser :while) (while-statement parser))
        (t (expression-statement parser))))

(defmethod for-statement ((parser parser))
  (consume parser :left-paren "Expect '(' after for.")
  (let (initializer for-condition increment body)
    (setf initializer (cond ((match parser :semicolon) nil)
                            ((match parser :var) (var-declaration parser))
                            (t (expression-statement parser))))

    (setf for-condition (if (not (check parser :semicolon))
                            (expression parser)
                            nil))
    (consume parser :semicolon "Expect ';' after loop condition")

    (setf increment (if (not (check parser :right-paren))
                        (expression parser)
                        nil))
    (consume parser :right-paren "Expect ')' after for clauses.")

    (setf body (statement parser))

    (when increment 
      (setf body 
            (block-stmt 
              :statements 
              (list body (expression-stmt :expression increment)))))

   
    (when (null for-condition)
      (setf for-condition (literal-expr :value t)))
    (setf body (while-stmt :while-condition for-condition :body body))
    
    (when initializer
      (setf body (block-stmt 
                   :statements
                   (list initializer body))))))

(defmethod if-statement ((parser parser))
  (consume parser :left-paren "Expect '(' after 'if'.")
  (let ((condition (expression parser)))
    (consume parser :right-paren "Expect ')' after if condition.")
    (let ((then-branch (statement parser))
          else-branch)
      (when (match parser :else)
        (setf else-branch (statement parser)))

      (if-stmt :if-condition condition 
               :then-branch then-branch 
               :else-branch else-branch))))

(defmethod print-statement ((parser parser))
  (let ((value (expression parser)))
    (consume parser :semicolon "Expect ';' after value.")
    (print-stmt :expression value)))

(defmethod var-declaration ((parser parser))
  (let ((name (consume parser :identifier "Expect variable name."))
        initializer)
    (when (match parser :equal)
        (setf initializer (expression parser)))

    (consume parser :semicolon "Expect ';' after variable declaration.")
    (var-stmt :name name :initializer initializer)))

(defmethod while-statement ((parser parser))
  (consume parser :left-paren "Expect '(' after while.")
  (let ((condition (expression parser)))
    (consume parser :right-paren "Expect ')' after condition.")
    (let ((body (statement parser)))
      (while-stmt :while-condition condition :body body))))

(defmethod expression-statement ((parser parser))
  (let ((expr (expression parser)))
    (consume parser :semicolon "Expect ';' after expression")
    (expression-stmt :expression expr)))

(defmethod parse-block ((parser parser))
  (let (statements)
    (while (and (not (check parser :right-brace))
                (not (at-end-p parser)))
      (push (parse-declaration parser) statements))
    (consume parser :right-brace "Expect '}' after block.")
    (nreverse statements)))

(defmethod assignment ((parser parser)) 
  (let ((expr (logical-or parser)))
    (when (match parser :equal)
      (let ((equals (previous parser))
            (value (assignment parser)))
        (when (equal (type-of expr) 'variable-expr)
          (let ((name (name expr)))
            (return-from assignment (assign-expr :name name :value value))))
        (error "Invalid assignment target")))
    expr))

(defmethod logical-or ((parser parser))
  (let ((expr (logical-and parser)))
    (while (match parser :or)
      (let ((operator (previous parser))
            (right (logical-and parser)))
        (setf expr (logical-expr :left expr :operator operator :right right))))
    expr))

(defmethod logical-and ((parser parser))
  (let ((expr (equality parser)))
    (while (match parser :and)
      (let ((operator (previous parser))
            (right (equality parser)))
        (setf expr (logical-expr :left expr :operator operator :right right))))
    expr))

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
  "Parse a unary expression"
  (if (match parser :bang :minus)
      (let ((operator (previous parser))
            (right (unary parser)))
        (unary-expr :operator operator :right right))
      (call parser)))

(defmethod call ((parser parser))
  "Parse a call expression"
  (let ((expr (primary parser)))
    (block finish-call-loop 
        (while t
            (if (match parser :left-paren)
                (setf expr (finish-call parser expr))
                (return-from finish-call-loop expr))))))

(defmethod finish-call ((parser parser) callee)
  "Finish parsing the rest of the function call including the arguments."
  (let (arguments paren)
    (when (not (check parser :right-paren))
      (push (expression parser) arguments)
      (while (match parser :comma)
        ; Limiting arguments to 255. This shouldn't be necessary in SBCL 
        ; not sure of other implementations
        (when (>= (length arguments) 255)
          (lox-error (peek parser) "Can't have more than 255 arguments."))
        (push (expression parser) arguments)))
    (setf paren (consume parser :right-paren "Expect ')' after arguments."))
    (call-expr :callee callee :paren paren :arguments arguments)))

(defmethod primary ((parser parser))
  (cond ((match parser :false) (literal-expr :value nil)) ; nil is false in common lisp
        ((match parser :true) (literal-expr :value t))
        ((match parser :nil) (literal-expr :value nil))
        ((match parser :number :string) 
         (literal-expr :value (literal (previous parser))))
        ((match parser :identifier) 
         (variable-expr :name (previous parser)))
        ((match parser :left-paren) 
         (let ((expr (expression parser)))
           (consume parser :right-paren "Expect ')' after expression.")
           (grouping-expr :expression expr)))
        (t (error (signal-error (peek parser) "Expect expression")))))

(defmethod match ((parser parser) &rest token-types)
  "Advances parser if it current token matches one of given token types"
  (when (some (lambda (token-type)
                 (check parser token-type))
              token-types)
    (advance parser)))

(defmethod consume ((parser parser) token-type message) 
  "Consume and return token if current token matches token-type.
   Throws an error with the given message otherwise."
  (if (check parser token-type)
      (advance parser)
      (error (signal-error (peek parser) message))))

(defmethod check ((parser parser) token-type)
  "Check if the current token matches given token-type"
  (if (at-end-p parser) 
      nil
      (equal token-type (token-type (peek parser)))))

(defmethod advance ((parser parser))
  "Returns current token and advances parser to the next token"
  (unless (at-end-p parser) (incf (current parser)))
  (previous parser))

(defmethod at-end-p ((parser parser))
  "Are we out of tokens?"
  (equal (token-type (peek parser)) :eof))

(defmethod peek ((parser parser))
  "Return current token without advancing parser"
  (aref (tokens parser) (current parser)))

(defmethod previous ((parser parser))
  "Return the previous token"
  (aref (tokens parser) (1- (current parser))))

(defun signal-error (token message) 
  "Signals a lox-error and creates a parser-error"
  (lox-error token message)
  (make-condition 'parse-error))

(defmethod synchronize ((parser parser))
  "Advance parser to the next valid state to resume parsing"
  (advance parser)
  (while (not (at-end-p parser))
    (when (equal (token-type (previous parser)) :semicolon)
      (return-from synchronize))
    (case (token-type (peek parser))
      ((:class :fun :var :for :if :while :print :return) (return-from synchronize))))
  (advance parser))
