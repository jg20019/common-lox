(in-package #:common-lox.scanning)

(defparameter *keywords* 
  (serapeum:dict 
    "and"    :and
    "class"  :class
    "else"   :else
    "false"  :false
    "for"    :for
    "fun"    :fun
    "if"     :if
    "nil"    :nil
    "or"     :or
    "print"  :print
    "return" :return
    "super"  :super
    "this"   :this
    "true"   :true
    "var"    :var
    "while"  :while))

(defclass scanner () 
  ((source :initarg :source :reader source)
   (tokens :initform '() :accessor tokens)
   (start :initform 0 :accessor start)
   (current :initform 0 :accessor current)
   (line :initform 1 :accessor line)))

(defun scanner (&key source) 
  (make-instance 'scanner :source source))


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
      (#\" (scan-string a-scanner))
      (otherwise 
        (cond ((digit-char-p c) (scan-number a-scanner))
              ((alphap c) (scan-identifier a-scanner))
              (t 
               (lox-error (line a-scanner) "Unexpected character.")))))))

(defmethod scan-identifier ((a-scanner scanner))
  (loop while (alphanump (peek a-scanner)) do (advance a-scanner))
  (with-slots (source start current) a-scanner
    (let* ((text (subseq source start current))
           (token-type (gethash text *keywords*)))
      (add-token a-scanner (if (null token-type) :identifier token-type)))))

(defmethod scan-string ((a-scanner scanner))
  (loop while (and (not (at-end-p a-scanner))
                   (char/= (peek a-scanner) #\"))
        do (progn 
             (when (char= (peek a-scanner) #\Newline)
               (incf (line a-scanner)))
             (advance a-scanner)))

  (when (at-end-p a-scanner)
    (lox-error (line a-scanner) "Unterminated string."))

  ; The closing "
  (advance a-scanner)

  (with-slots (source start current) a-scanner
    (let ((value (subseq source (+ start 1) (- current 1))))
      (add-token a-scanner :string value))))

(defmethod scan-number ((a-scanner scanner))
  (loop while (digit-char-p (peek a-scanner))
        do (advance a-scanner))

  ; look for the fractional part
  (when (and (char= #\. (peek a-scanner))
           (digit-char-p (peek-next a-scanner)))
      ; consume the "."
      (advance a-scanner)
      
      (loop while (digit-char-p (peek a-scanner))
            do (advance a-scanner)))

  (with-slots (source start current) a-scanner
    (add-token a-scanner :number
               (serapeum:parse-float (subseq source start current)))))

(defmethod peek ((a-scanner scanner))
  (with-slots (source current) a-scanner 
    (if (at-end-p a-scanner) 
        #\Nul
        (aref source current))))

(defmethod peek-next ((a-scanner scanner))
  (with-slots (source current) a-scanner 
    (if (>= (+ current 1) (length source)) 
        #\Nul
        (aref source (+ current 1)))))

(defun alphap (c)
  (or (char= #\_ c)
      (alpha-char-p c)))

(defun alphanump (c)
  (or (alphap c) (digit-char-p c)))

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
