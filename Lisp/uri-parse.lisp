(defun identificator% (list &optional delimitators eol accumulator)
  (when list
    (if (and (null (cdr list)) (not (member (first list) delimitators)) eol) (values (nreverse (cons (car list) accumulator)) nil)
      (if (member (first list) delimitators)
          (values (nreverse accumulator) list)
          (identificator% (rest list) delimitators eol
              (cons (car list) accumulator)))
      )
    ))

; "abc" c -> "bc" c -> 
; identificator% "abc" "b" -> (a) (c)
; identificator% nil "b" -> nil
; identificator% "abc" "d" -> nil

(defun identificator (list delimitator &optional eol)
  (let ((parse (multiple-value-list (identificator% list delimitator eol))))
    (if (first parse)
        (values-list parse)
      (values nil list))))

(defun scheme (list)
  (multiple-value-bind (schema rest)
    (identificator list '(#\:))
    (values (cons 'uri-scheme schema) rest)
  )
)

(defun userinfo (list)
  (multiple-value-bind (schema rest)
    (identificator list '(#\@))
    (if (null schema) (values (list 'uri-userinfo nil) rest)
      (values (cons 'uri-userinfo schema) (cdr rest)))))

(defun host (list)
(multiple-value-bind (uri-host rest)
    (identificator list '(#\: #\/) T)
     (if (null uri-host) (values (list 'uri-host nil) rest)
      (values (cons 'uri-host uri-host) rest))
  )
)

(defun port (list)
  (cond
    ((eq (first list) #\:) (list 'uri-port 80))
    (T (values (list 'uri-port nil) list))
  )
)

(defun authority (list)
  (cond 
    ((and (eq (first list) #\/) (eq (second list) #\/)) 
      (let* (
        (parsedUserInfo (multiple-value-list (userinfo (rest (rest list)))))
        (parsedHost (multiple-value-list (host (second parsedUserInfo))))
        (parsedPort (multiple-value-list (port (second parsedHost)))))
        (list (first parsedUserInfo) (first parsedHost) (first parsedPort) (second parsedPort))
      ))
  )
)

(defun parse (InputString)
  (let* (
    (parsedSchema (multiple-value-list (scheme (coerce InputString 'list))))
    (parsedAuthority (multiple-value-list (authority (rest (second parsedSchema))))))
    (list (first parsedSchema) parsedAuthority)
  )
)