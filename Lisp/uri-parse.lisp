; Definizione delle Classi
  (:constructor create-uri-scheme (value)))
  value
)

(defstruct (uri_userinfo
  (:constructor create-uri-userinfo (value)))
  value
)

(defstruct (uri_host
  (:constructor create-uri-host (value)))
  value
)

(defstruct (uri_port
  (:constructor create-uri-port (value)))
  value
)

(defstruct (uri_authority 
  (:constructor create-uri-authority (uri_userinfo uri_host uri_port)))
  uri_userinfo
  uri_host
  uri_port
)

(defstruct (uri_component
  (:constructor create-uri-component (
    uri_scheme
    uri_authority
    uri_path
    uri_query
    uri_fragment
  )))
  uri_scheme
  uri_authority
  uri_path
  uri_query
  uri_fragment
)

(defclass schema ()
  ((value
    :initarg :value
    :accessor value)
  )
)

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
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\:))
    (values (create-uri-scheme (coerce parsed 'string)) (rest remaining))
  )
)

(defun parse-userinfo (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\@))
    (values (create-uri-userinfo (coerce parsed 'string)) remaining)
  )
)

(defun parse-host (list)
  (multiple-value-bind (parsed remaining)
      (identificator list '(#\: #\/ eof))
      (values (create-uri-host (coerce parsed 'string)) remaining)
    )
)

(defun parse-port (list)
   (if (not (eq (first list) #\:))
      (values (list 'uri-port) list)
      (multiple-value-bind (parsed remaining)
        (identificator list '(#\/ eof))
        (values (create-uri-port (coerce parsed 'string)) remaining)
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

; Restituisce un oggetto composed che contiene userinfo, host, port e il resto dell'input da parsare
(defun parse-authority (URIStringList)
  (let* (
    (parsed_userinfo (multiple-value-list (parse-userinfo URIStringList)))
    (parsed_host (multiple-value-list (parse-host (second parsed_userinfo))))
    (parsed_port (multiple-value-list (parse-port (second parsed_host)))))
    (values (create-uri-authority (first parsed_userinfo) (first parsed_host) (first parsed_port)) (second parsed_port))
  )
)

; scheme ‘:’ authorithy [‘/’ [path] [‘?’ query] [‘#’ fragment]]
(defun parse-default-uri-type (URIStringList)
  (let* (
    (parsed_authority (multiple-value-list (parse-authority URIStringList)))
    (parsed_path (multiple-value-list (parse-path (second parsed_authority))))
    (parsed_query (multiple-value-list (parse-query (second parsed_path))))
    (parsed_fragment (multiple-value-list (parse-fragment (second parsed_query)))))
    (list 
      (first parsed_authority) 
      (first parsed_path) 
      (first parsed_query) 
      (first parsed_fragment)
    )
  )
)

; scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
; TODO Gestire il slash opzionale
(defun parse-resource-uri-type (URIStringList)
  (let* (
    (parsed_path (multiple-value-list (parse-path URIStringList)))
    (parsed_query (multiple-value-list (parse-query (second parsed_path))))
    (parsed_fragment (multiple-value-list (parse-fragment (second parsed_query)))))
    (list 
      (list 'uri-authority) 
      (first parsed_path) 
      (first parsed_query) 
      (first parsed_fragment)
    )
  )
)

; Chiama la funzione corretta in base a cosa abbiamo dopo "scheme : "
(defun parse-uri-type (URIStringList)
  (let (
    (parsed_scheme (multiple-value-list (parse-scheme URIStringList))))
    (cond
      ((and
        (char= (first (second parsed_scheme)) #\/)
        (char= (second (second parsed_scheme)) #\/)) 
        (append (first parsed_scheme) (parse-default-uri-type (cdr (cdr (second parsed_scheme)))))
      )
      (T 
        (append (first parsed_scheme) (parse-resource-uri-type (second parsed_scheme)))
      )
    )
  )
)

; Utility interna, aggiunge EOF alla fine della lista come delimitatore
; Richiama una funzione che gestisce i vari tipi di URI
(defun uri-parse_ (URIStringList)
  (let (
    (parsed_input (append URIStringList '(eof))))
    (let (
        (parsed_uri (multiple-value-list (parse-uri-type parsed_input))))
        (create-uri-component 'uri-component parsed_uri)
    )
  )
)

; Funzione principale, converta la stringa in Input come una lista di caratteri
(defun uri-parse (URIString)
  (uri-parse_ (coerce URIString 'list))
)
