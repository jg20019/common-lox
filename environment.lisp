(in-package :common-lox)

(defclass environment () 
  ((enclosing 
     :initform nil
     :initarg :enclosing
     :reader enclosing)
   (env-values 
    :initform (serapeum:dict)
    :accessor env-values)))

(defun new-environment (&optional (enclosing nil))
  (make-instance 'environment :enclosing enclosing))

(defmethod define ((env environment) name value)
  (setf (gethash name (env-values env)) value))

(defmethod assign ((env environment) name value)
  (multiple-value-bind (_ foundp) (gethash (lexeme name) (env-values env))
    (declare (ignore _))
    (cond (foundp (setf (gethash (lexeme name) (env-values env)) value))
          ((enclosing env) (assign (enclosing env) name value))
          (t (error 'runtime-error
                    :token name
                    :message (format nil "Undefined variable '~a'." (lexeme name)))))))

(defmethod getValue ((env environment) name)
  (multiple-value-bind (value foundp) (gethash (lexeme name) (env-values env))
    (cond (foundp value)
          ((enclosing env) (getValue (enclosing env) name))
          (t (error 'runtime-error 
                    :token name 
                    :message (format nil "Undefined variable '~a'." (lexeme name)))))))
