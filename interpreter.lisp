(in-package #:common-lox)

(defclass interpreter () ())

(defmethod interpret ((interpreter interpreter) (expression expr))
  (handler-case 
    (format t "~a~%" (stringify (evaluate expression)))
    (runtime-error (e) 
                   (runtime-error e))))

(defun interpreter () 
  (make-instance 'interpreter))

(defun stringify (object) 
  (cond ((null object) "nil")
        ((numberp object) (let ((text (format nil "~a" object)))
                            (if (str:ends-with-p ".0" text)
                               (subseq text 0 (- (length text) 2))
                               text)))
        (t (format nil "~a" object))))
