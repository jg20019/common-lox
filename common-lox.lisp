;;;; common-lox.lisp

(in-package #:common-lox)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *had-error* nil "Did the interpreter encounter an error?")
(defparameter *had-runtime-error* nil "Did the interpreter encounter a runtime error?")

(defun run-file (path) 
  (with-open-file (input path :direction :input) 
    (run (uiop:read-file-string path))
    (when *had-error* :error)
    (when *had-runtime-error* :runtime-error)))

(defun run-prompt () 
  (loop do (progn 
             (format t "~&> ")
             (finish-output)
             (let ((line (read-line)))
               (when (or (null line) (string-equal line "")) (return))
               (run line)
               (setf *had-error* nil)))))

(defun run (source) 
  (let* ((scanner (common-lox.scanning:scanner :source source))
         (tokens (common-lox.scanning:scan-tokens scanner))
         (parser (common-lox.parsing:parser (coerce  tokens 'vector)))
         (expression (common-lox.parsing:parse parser))
         (interpreter (interpreter)))
    (when *had-error* (return-from run))
    (interpret interpreter expression)
    (finish-output)))

(defmethod lox-error ((token token) message) 
  (if (equal (token-type token) :eof)
      (report (line token) " at end" message)
      (report (line token) 
              (format nil " at '~s'" (lexeme token))
              message)))

(defmethod lox-error ((line number) message) 
  (report line "" message))

(defun runtime-error (error) 
  (format *error-output* "~a~%[line ~a]" (message error) (line (token error)))
  (finish-output))

(defun report (line where message) 
  (format *error-output* "~&[line ~a] Error~a: ~a" line where message)
  (setf *had-error* t))
