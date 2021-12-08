(defun identificator% (list &optional delimitators accumulator)
  (when list
    (if (member (first list) delimitators)
    (values (nreverse accumulator) list)
      (identificator% (rest list)
              delimitators
              (cons (car list) accumulator)))))

(defun identificator (list delimitator)
  (let ((parse (multiple-value-list (identificator% list delimitator))))
    (if (first parse)
        (values-list parse)
      (values nil list))))

(defun parse-scheme (list)
  (multiple-value-bind (schema rest)
    (identificator list '(#\:))
    (values (cons 'uri-scheme schema) (cdr rest))
  )
)

(defun parse-userinfo (list)
  (multiple-value-bind (schema rest)
    (identificator list '(#\@))
    (values (cons 'uri-userinfo schema) rest)
  )
)

(defun parse-host (list)
  (multiple-value-bind (schema rest)
      (identificator list '(#\: #\/ eof))
      (values (cons 'uri-host schema) rest)
    )
)

(defun parse-port (list)
   (if (not (eq (first list) #\:))
      (values (list 'uri-port) list)
      (multiple-value-bind (parsed remaining)
        (identificator list '(#\/ eof))
        (values (list 'uri-port (rest parsed)) remaining)
      )
  )
)

(defun parse-path (list)
  (multiple-value-bind (parsed rest)
    (identificator list '(#\? #\# eof))
    (values (list 'uri-path parsed) rest)
  )
)

(defun parse-query (list)
  (if (not (eq (first list) #\?))
    (values (list 'uri-query) list)
    (multiple-value-bind (parsed rest)
      (identificator list '(#\# eof))
      (values (list 'uri-query parsed) rest)
    )
  )
)

(defun parse-fragment (list)
  (if (not (eq (first list) #\#))
      (values (list 'uri-fragment) list)
      (multiple-value-bind (parsed rest)
        (identificator list '(eof))
        (values (list 'uri-fragment parsed) rest)
    )
  )
)

(defun parse-authority (URIStringList)
  (let* (
    (parsed_userinfo (multiple-value-list (parse-userinfo URIStringList)))
    (parsed_host (multiple-value-list (parse-host (second parsed_userinfo))))
    (parsed_port (multiple-value-list (parse-port (second parsed_host)))))
    (values (list (first parsed_userinfo) (first parsed_host) (first parsed_port)) (second parsed_port))
  )
)

(defun parse-default-type (URIStringList)
  (let* (
    (parsed_authority (multiple-value-list (parse-authority URIStringList)))
    (parsed_path (multiple-value-list (parse-path (second parsed_authority))))
    (parsed_query (multiple-value-list (parse-query (second parsed_path))))
    (parsed_fragment (multiple-value-list (parse-fragment (second parsed_query)))))
    (list (first parsed_authority) (first parsed_path) (first parsed_query) (first parsed_fragment))
  )
)

(defun parse-uri-type (URIStringList)
  (let (
    (parsed_scheme (multiple-value-list (parse-scheme URIStringList))))
    (cond
      ((and
        (char= (first (second parsed_scheme)) #\/)
        (char= (second (second parsed_scheme)) #\/)) 
          (append (first parsed_scheme) (parse-default-type (cdr (cdr (second parsed_scheme)))))
      )
    )
  )
)

(defun uri-parse_ (URIStringList)
  (let (
    (parsed_input (append URIStringList '(eof)))) ; Appendo EOF a fine stringa per usarlo come delimitatore
    (let (
        (parsed_uri (multiple-value-list (parse-uri-type parsed_input))))
        (list 'uri-component parsed_uri)
    )
  )
)

(defun uri-parse (URIString)
  (uri-parse_ (coerce URIString 'list))
)
