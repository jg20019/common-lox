(in-package :common-lox)

(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort)
     (progn ,@body)))

(defun main* (args) 
  (cond ((> (length args) 1) 
         (format t "Usage: common-lox [script]")
         :exit) 
        ((= (length args) 1)
         (run-file (first args)))
        (t (run-prompt))))

(defparameter *ui* 
  (adopt:make-interface 
    :name "clox"
    :summary "Execute clox code"
    :usage "[FILE]"
    :help "Interpret file or enter repl if no file is given"))

(defun main ()
  (sb-ext:disable-debugger)
  (handler-case 
    (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
      (declare (ignore options))
      (main* arguments))
    (error (c)
           (adopt:print-error-and-exit c))))
