(defclass Scheme ()
    (value)
)

(defun identificator (InputList DelimitatorList)
  (if (null InputList) nil 
    (if (member (first InputList) DelimitatorList) (values nil (cdr InputList))
    (multiple-value-bind (result rest)
      (identificator (cdr InputList) DelimitatorList)
      (values (cons (car InputList) result) rest)
    )
  )
))

(defun scheme (InputList)
  (if (null InputList) nil 
    (multiple-value-bind (scheme rest)
      (identificator InputList '(#\:))
      (values scheme rest)
    )
  )
)

(defun parse (InputString)
  (scheme (coerce InputString 'list))
)
