(defun identificator% (list &optional delimitators accumulator)
  (when list
    (if (member (first list) delimitators)
    (values (nreverse accumulator) (rest list))
      (identificator% (rest list)
              delimitators
              (cons (car list) accumulator)))))

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