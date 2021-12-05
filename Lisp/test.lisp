(defclass Scheme ()
    (value)
)

(defun identificator (InputList &optional Delimitator)
  (if (null InputList) nil 
    (if (eq (first InputList) Delimitator) (values nil (cdr InputList))
    (multiple-value-bind (result rest)
      (identificator (cdr InputList) Delimitator)
      (values (cons (car InputList) result) rest)
    )
  )
))

(defun scheme (InputList)
  (if (null InputList) nil 
    (multiple-value-bind (scheme rest)
      (identificator InputList #\:)
      (values scheme rest)
    )
  )
)

(defun parse (InputString)
  (scheme (coerce InputString 'list))
)