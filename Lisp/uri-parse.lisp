(define-condition uri-invalid (error) ())
; Definizione delle Classi
(defclass schema ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass userinfo ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass host ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass port ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass path ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass query ()
  ((value
    :initarg :value
    :accessor value)
  ))

(defclass fragment ()
  ((value
    :initarg :value
    :accessor value)
  ))

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
   ))

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
  (when uri-struct (value (get-schema uri-struct)))
 )

(defun uri-userinfo (uri-struct)
  (when uri-struct (cond ((null (get-userinfo (get-authority uri-struct))) nil)
        (T (value (get-userinfo (get-authority uri-struct))))))
 )

(defun uri-host (uri-struct)
  (when uri-struct (cond ((null (get-host (get-authority uri-struct))) nil)
        (T (value (get-host (get-authority uri-struct))))))
 )

(defun uri-port (uri-struct)
  (when uri-struct (cond ((null (get-port (get-authority uri-struct))) nil)
        (T (value (get-port (get-authority uri-struct))))))
 )

(defun uri-path (uri-struct)
  (when uri-struct (cond ((null (get-path uri-struct)) nil)
        (T (value (get-path uri-struct)))))
 )

(defun uri-query (uri-struct)
  (when uri-struct (cond ((null (get-query uri-struct)) nil)
        (T (value (get-query uri-struct)))))
 )

(defun uri-fragment (uri-struct)
  (when uri-struct (cond ((null (get-fragment uri-struct)) nil)
        (T (value (get-fragment uri-struct)))))
 )

(defun identificator% (list &optional delimitators banned accumulator)
  (when list
    (if (member (first list) delimitators)
     (values (nreverse accumulator) list)
     (when (not (member (first list) banned))
      (identificator% (rest list)
                      delimitators
                      banned
                      (cons (car list) accumulator))))))

(defun identificator (list delimitator &optional banned)
  (let ((parse (multiple-value-list (identificator% list delimitator banned))))
    (if (first parse)
        (values-list parse)
      (values nil list))))

(defun identificator-host (URIHostList)
  (cond 
    ((or (eq (first URIHostList) 'eof) (member (first URIHostList) '(#\. #\/ #\? #\# #\@ #\:))) (error 'uri-invalid))
    (T (let ((parse (multiple-value-list (identificator URIHostList '(#\. #\/ #\: eof) '(#\? #\# #\@)))))
    (if (eq (first (second parse)) #\.) ; abbiamo un subhost
        (let ((secondParse (multiple-value-list (identificator-host (cdr (second parse))))))
              (values (append (first parse) '(#\.) (first secondParse)) (second secondParse)))  
      (values-list parse))))
  )
)

(defun parse-scheme (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\:) (coerce "/?#@" 'list))
    (values (make-instance 'schema :value (coerce parsed 'string)) (rest remaining))))

(defun parse-userinfo (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\@) (coerce "/?#" 'list))
    (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              (cdr remaining))))))

(defun parse-host (list)
  (multiple-value-bind (parsed remaining)
      (identificator-host list)
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
    (parsed-userinfo (multiple-value-list (parse-userinfo URIStringList)))
    (parsed-host (multiple-value-list (parse-host (second parsed-userinfo))))
    (parsed-port (multiple-value-list (parse-port (second parsed-host)))))
    (values 
     (make-instance 'authority 
      :userinfo (first parsed-userinfo) 
      :host (first parsed-host) 
      :port (first parsed-port)) 
     (second parsed-port))
  )
)

; scheme ‘:’ authorithy [‘/’ [path] [‘?’ query] [‘#’ fragment]]
(defun parse-default-uri (URIStringList)
  (let* (
    (parsed-authority (multiple-value-list (parse-authority URIStringList)))
    (parsed-path (multiple-value-list (parse-path (second parsed-authority))))
    (parsed-query (multiple-value-list (parse-query (second parsed-path))))
    (parsed-fragment (multiple-value-list (parse-fragment (second parsed-query)))))
    (list 
      (first parsed-authority) 
      (first parsed-path) 
      (first parsed-query) 
      (first parsed-fragment)
    )
  )
)

; scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
; TODO Gestire il slash opzionale
(defun parse-resource-uri (URIStringList)
  (let* (
    (parsed-path (multiple-value-list (parse-path URIStringList)))
    (parsed-query (multiple-value-list (parse-query (second parsed-path))))
    (parsed-fragment (multiple-value-list (parse-fragment (second parsed-query)))))
    (list 
      (list 'uri-authority) 
      (first parsed-path) 
      (first parsed-query) 
      (first parsed-fragment)
    )
  )
)

; Chiama la funzione corretta in base a cosa abbiamo dopo "scheme : "
(defun parse-uri-type (URIStringList)
  (let (
    (parsed-scheme (multiple-value-list (parse-scheme URIStringList))))
    (cond
      ((and
        (char= (first (second parsed-scheme)) #\/)
        (char= (second (second parsed-scheme)) #\/))
        (let 
          ((otherComp (parse-default-uri (cdr (cdr (second parsed-scheme))))))
          (make-instance 'uri-structure :schema (first parsed-scheme) 
                                        :authority (first otherComp)
                                        :path (second otherComp)
                                        :query (third otherComp)
                                        :fragment (fourth otherComp))))
      (T 
        (append (first parsed-scheme) (parse-resource-uri (second parsed-scheme)))
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
  (handler-case (uri-parse_ (coerce URIString 'list))
    (error ()
      (format t "URI non valido")
  ))
)
