(in-package :common-lox)

(defclass environment () 
  ((env-values 
    :initform (serapeum:dict )
    :accessor env-values)))

(defmethod define ((env environment) name value)
  (setf (gethash name (env-values env)) value))

(defmethod assign ((env environment) name value)
  (multiple-value-bind (_ foundp) (gethash (lexeme name) (env-values env))
    (declare (ignore _))
    (if foundp 
        (setf (gethash (lexeme name) (env-values env)) value)
        (error 'runtime-error
               :token name
               :message (format nil "Undefined variable '~a'." (lexeme name))))))

(defmethod getValue ((env environment) name)
  (multiple-value-bind (value foundp) (gethash (lexeme name) (env-values env))
    (if foundp 
        value
        ; throws a runtime-error here but it looks a little different than 
        ; the one I may have defined
        (error 'runtime-error 
               :token name 
               :message (format nil "Undefined variable '~a'." (lexeme name))))))
