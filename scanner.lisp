(in-package #:common-lox)

(defclass scanner () 
  ((source :initarg :source :reader source)
   (tokens :initform '() :accessor tokens)
   (start :initform 0 :accessor start)
   (current :initform 0 :accessor current)
   (line :initform 1 :accessor line)))


(defmethod at-end-p ((a-scanner scanner))
  "Check if scanner is at the end"
  (>= (current a-scanner) (length (source a-scanner))))

(defmethod scan-tokens ((a-scanner scanner))
  (loop while (not (at-end-p a-scanner))
        do (progn 
             (setf (start a-scanner) (current a-scanner))
             (scan-token a-scanner)))
  (push (make-instance 'token :type :eof :lexeme "" :literal nil :line (line a-scanner)) (tokens a-scanner))
  (reverse (tokens a-scanner)))

(defmethod scan-token ((a-scanner scanner))
  (let ((c (advance a-scanner)))
    (case c 
      (#\( (add-token a-scanner :left-paren))
      (#\) (add-token a-scanner :right-paren))
      (#\{ (add-token a-scanner :left-brace))
      (#\} (add-token a-scanner :right-brace))
      (#\, (add-token a-scanner :comma))
      (#\. (add-token a-scanner :dot))
      (#\+ (add-token a-scanner :plus))
      (#\- (add-token a-scanner :minus))
      (#\; (add-token a-scanner :semicolon))
      (#\* (add-token a-scanner :star))
      (#\! (add-token a-scanner (if (match a-scanner #\=) :bang-equal :bang)))
      (#\= (add-token a-scanner (if (match a-scanner #\=) :equal-equal :equal)))
      (#\< (add-token a-scanner (if (match a-scanner #\=) :less-equal :less)))
      (#\> (add-token a-scanner (if (match a-scanner #\=) :greater-equal :greater)))
      (#\/ (cond ((match a-scanner #\/) (loop while (and (not (at-end-p a-scanner))
                                                         (char/= (peek a-scanner) #\newline))
                                              do (advance a-scanner)))
                 (t (add-token a-scanner :slash))))
      ((#\Space #\Tab) nil) ; Ignore spaces and tabs
      (#\Newline (incf (line a-scanner)))
      (otherwise (lox-error (line a-scanner) "Unexpected character.")))))

(defmethod peek ((a-scanner scanner))
  (with-slots (source current) a-scanner 
    (if (at-end-p a-scanner) 
        #\Nul
        (aref source current))))

(defmethod advance ((a-scanner scanner))
  "Returns current character and then advances current"
  (let ((index (current a-scanner)))
    (incf (current a-scanner))
    (aref (source a-scanner) index)))

(defmethod match ((a-scanner scanner) expected)
  "Check if current character matches expected character"
  (cond ((at-end-p a-scanner) nil)
        ((char/= (aref (source a-scanner) (current a-scanner)) expected) nil)
        (t (incf (current a-scanner)) t)))

(defmethod add-token ((a-scanner scanner) type &optional (literal nil))
  (with-slots (source start current tokens line) a-scanner 
    (let ((text (subseq source start current)))
      (push (make-instance 'token :type type :lexeme text :literal literal :line line) tokens))))
