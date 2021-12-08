; Definizione delle Classi
(defclass schema ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass userinfo ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass host ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass port ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass path ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass query ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass fragment ()
  ((value
    :initarg :value
    :accessor value)
  )
)

(defclass authority ()
  (
    (userinfo
     :initarg :userinfo
     :initform nil
     :accessor get-userinfo)
    (host
     :initarg :host
     :initform nil
     :accessor get-host)
    (port
     :initarg :port
     :initform nil
     :accessor get-port)
   )
)

(defclass uri-structure ()
 (
  (schema
   :initarg :schema
   :accessor get-schema
   )
  (authority
   :initarg :authority
   :initform nil
   :accessor get-authority
   )
  (path
   :initarg :path
   :initform nil
   :accessor get-path
   )
  (query
   :initarg :query
   :initform nil
   :accessor get-query
   )
  (fragment
   :initarg :fragment
   :initform nil
   :accessor get-fragment
   )
  )
 )

(defun uri-scheme (uri-struct)
  (value (get-schema uri-struct))
 )

(defun uri-userinfo (uri-struct)
  (cond ((null (get-userinfo (get-authority uri-struct))) nil)
        (T (value (get-userinfo (get-authority uri-struct)))))
 )

(defun uri-host (uri-struct)
  (cond ((null (get-host (get-authority uri-struct))) nil)
        (T (value (get-host (get-authority uri-struct)))))
 )

(defun uri-port (uri-struct)
  (cond ((null (get-port (get-authority uri-struct))) nil)
        (T (value (get-port (get-authority uri-struct)))))
 )

(defun uri-path (uri-struct)
  (cond ((null (get-path uri-struct)) nil)
        (T (value (get-path uri-struct))))
 )

(defun uri-query (uri-struct)
  (cond ((null (get-query uri-struct)) nil)
        (T (value (get-query uri-struct))))
 )

(defun uri-fragment (uri-struct)
  (cond ((null (get-fragment uri-struct)) nil)
        (T (value (get-fragment uri-struct))))
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
    (values (make-instance 'schema :value (coerce parsed 'string)) (rest remaining))
  )
)

(defun parse-userinfo (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\@))
    (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              (cdr remaining))))))

(defun parse-host (list)
  (multiple-value-bind (parsed remaining)
      (identificator list '(#\: #\/ eof))
      (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'host :value (coerce parsed 'string)) 
              remaining))
     )
    )
)

(defun parse-port (list)
   (if (not (eq (first list) #\:))
      (values nil list)
      (multiple-value-bind (parsed remaining)
        (identificator list '(#\/ eof))
        (cond ((null parsed) 
            (values nil remaining)
           )
          ((every #'digit-char-p (coerce parsed 'string)) (values 
              (make-instance 'port :value (coerce parsed 'string)) 
              (cdr remaining)))
        )
      )
  )
)

(defun parse-path (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\? #\# eof))
    (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'path :value (coerce parsed 'string)) 
              remaining))
     )
  )
)

(defun parse-query (list)
  (if (not (eq (first list) #\?))
    (values nil list)
    (multiple-value-bind (parsed remaining)
      (identificator (cdr list) '(#\# eof))
      (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              remaining))
     )
    )
  )
)

(defun parse-fragment (list)
  (if (not (eq (first list) #\#))
      (values nil list)
      (multiple-value-bind (parsed remaining)
        (identificator (cdr list) '(eof))
        (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              (cdr remaining)))
     )
    )
  )
)

; Restituisce un oggetto composed che contiene userinfo, host, port e il resto dell'input da parsare
(defun parse-authority (URIStringList)
  (let* (
    (parsed_userinfo (multiple-value-list (parse-userinfo URIStringList)))
    (parsed_host (multiple-value-list (parse-host (second parsed_userinfo))))
    (parsed_port (multiple-value-list (parse-port (second parsed_host)))))
    (values 
     (make-instance 'authority 
      :userinfo (first parsed_userinfo) 
      :host (first parsed_host) 
      :port (first parsed_port)) 
     (second parsed_port))
  )
)

; scheme ‘:’ authorithy [‘/’ [path] [‘?’ query] [‘#’ fragment]]
(defun parse-default-uri (URIStringList)
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
(defun parse-resource-uri (URIStringList)
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
        (let 
          ((otherComp (parse-default-uri (cdr (cdr (second parsed_scheme))))))
          (make-instance 'uri-structure :schema (first parsed_scheme) 
                                        :authority (first otherComp)
                                        :path (second otherComp)
                                        :query (third otherComp)
                                        :fragment (fourth otherComp))))
      (T 
        (append (first parsed_scheme) (parse-resource-uri (second parsed_scheme)))
      )
    )
  )
)

; Utility interna, aggiunge EOF alla fine della lista come delimitatore
; Richiama una funzione che gestisce i vari tipi di URI
(defun uri-parse_ (URIStringList)
  (parse-uri-type (append URIStringList '(eof)))
)

; Funzione principale, converta la stringa in Input come una lista di caratteri
(defun uri-parse (URIString)
  (uri-parse_ (coerce URIString 'list))
)
