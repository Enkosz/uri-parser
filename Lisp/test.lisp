#| (defun identificator% (lista delimitator)
  (cond (
    ((null lista) nil)
    ((eq (car lista) delimitator) (values nil (cdr lista)))
    (T (multiple-value-bind (left right)
      (identificator% (cdr lista) delimitator)
      ()
    ))
  ))
) |#

(defun without-last(l)
    (reverse (cdr (reverse l)))
    )

(defun identificator% (list &optional del)
  (cond ((null list) nil)
        ((not (member del list)) (values nil list))
        ((eq (first list) del) (values nil (cdr list)))
        (T (multiple-value-bind (result rest)
          (identificator% (cdr list) del); expr
          (values (cons (car list) result) rest); body
          ))
    )
  )

; "abc" c -> "bc" c -> 
; identificator% "abc" "b" -> (a) (c)
; identificator% nil "b" -> nil
; identificator% "abc" "d" -> nil

(defun identificator (list delimitator)
  (let ((parse (multiple-value-list (identificator% list delimitator))))
    (if (first parse)
        (values-list parse)
      (values nil (second parse)))))

(defun scheme (list)
  (multiple-value-bind (schema rest)
    (identificator list #\:)
    (values (cons 'uri-scheme schema) rest)
  )
)

(defun userinfo (list)
  (multiple-value-bind (schema rest)
    (identificator list #\@)
    (values (cons 'uri-userinfo schema) rest)
  )
)

(defun host (list)
(multiple-value-bind (schema rest)
    (identificator list #\:)
    (values (cons 'uri-host schema) rest)
  )
)

(defun authority (list)
  (cond 
    ((and (eq (first list) #\/) (eq (second list) #\/)) 
      (let* (
        (parsedUserInfo (multiple-value-list (userinfo list)))
        (parsedHost (multiple-value-list (host (second parsedUserInfo)))))
        (list (first parsedUserInfo) (first parsedHost))
      ))
  )
)

(defun parse (InputString)
  (let* (
    (parsedSchema (multiple-value-list (scheme (coerce InputString 'list))))
    (parsedAuthority (multiple-value-list (authority (second parsedSchema)))))
    (list (first parsedSchema) parsedAuthority)
  )
)