;;;; common-lox.lisp

(in-package #:common-lox)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *had-error* nil "Did the interpreter encounter an error?")

(defun main (args) 
  (cond ((> (length args) 1) 
         (format t "Usage: common-lox [script]")
         :exit) 
        ((= (length args) 1)
         (run-file (first args)))
        (t (run-prompt))))

(defun run-file (path) 
  (with-open-file (input path :direction :input) 
    (run (uiop:read-file-string path))
    (when *had-error* :exit)))

(defun run-prompt () 
  (loop do (progn 
             (format t "~&> ")
             (let ((line (read-line)))
               (when (or (null line) (string-equal line "")) (return))
               (run line)
               (setf *had-error* nil)))))

(defun run (source) 
  (let* ((scanner (common-lox.scanning:scanner :source source))
         (tokens (common-lox.scanning:scan-tokens scanner)))
    (dolist (token tokens) 
      (format t "~&~a" token))))

(defun lox-error (line message) 
  (report line "" message))

(defun report (line where message) 
  (format *error-output* "~&[line ~a] Error~a: ~a" line where message)
  (setf *had-error* t))
