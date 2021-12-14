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

(defun make-uri-authority (&optional userinfo host port)
  (make-instance 'authority 
      :userinfo userinfo
      :host host
      :port port))

(defun make-uri-structure (Scheme &optional authority path query fragment)
  (make-instance 'uri-structure 
    :schema Scheme
    :authority authority
    :path path
    :query query
    :fragment fragment))

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

(defun identificator-special (list delimitator delimitatorIdentificator &optional bannedIdentificator)
  (cond 
    ((member (first list) '(#\. #\/ #\? #\# #\@ #\: eof)) (error 'uri-invalid-identificatorSpecial))
    (T (let ((parse (multiple-value-list (identificator list delimitatorIdentificator bannedIdentificator))))
    (if (eq (first (second parse)) delimitator) ; abbiamo un subhost/subpath
        (let ((secondParse (multiple-value-list (identificator-special (cdr (second parse)) delimitator delimitatorIdentificator bannedIdentificator))))
              (values (append (first parse) (list delimitator) (first secondParse)) (second secondParse)))  
      (values-list parse))))
  )
)

(defun parse-scheme (list)
  (multiple-value-bind (parsed remaining)
    (identificator list '(#\:) (coerce "/?#@" 'list))
    (cond 
      ((null parsed) (error 'uri-invalid-scheme))
      (T (values (make-instance 'schema :value (coerce parsed 'string)) (rest remaining))))
    )
)

(defun parse-userinfo (list &optional delimitatorSpecial)
  (multiple-value-bind (parsed remaining)
    (identificator list (append '(#\@) delimitatorSpecial) (set-difference '(#\/ #\? #\# #\: #\Space) delimitatorSpecial))
    (cond ((null parsed) 
            (values nil remaining)
          )
          ((not (null delimitatorSpecial)) (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              remaining))
          (T (values 
              (make-instance 'userinfo :value (coerce parsed 'string)) 
              (cdr remaining))))))

(defun parse-host-aux (list)
  (multiple-value-bind (parsed remaining)
      (identificator-special list #\. '(#\. #\/ #\: eof) '(#\? #\# #\@ #\Space))
      (cond ((null parsed) 
            (error 'uri-invalid-host))
          (T (values 
              (make-instance 'host :value (coerce parsed 'string)) 
              remaining))
     )
    )
)

; inter. ---> ip --> host

(defun triplet (list &optional final)
  (let ((parse (multiple-value-list (identificator list '(#\. eof #\: #\/) '(#\@ #\? #\#  #\Space)))))
  (cond 
    ((null (first parse)) (error 'ip-non-valido))
    ((and final (eq (first (second parse)) #\.)) (error 'ip-non-valido))
    ((and (not final) (not(eq (first (second parse)) #\.))) (error 'ip-non-valido))
    (T (values (first parse) (second parse))))))

(defun parse-ip (list)
  (let* (
    (firstTriplet (multiple-value-list (triplet list)))
    (secondTriplet (multiple-value-list (triplet (cdr (second firstTriplet)))))
    (thirdTriplet (multiple-value-list (triplet (cdr (second secondTriplet)))))
    (fourhTriplet (multiple-value-list (triplet (cdr (second thirdTriplet)) T)))
    (num1 (parse-integer (coerce (first firstTriplet) 'string)))
    (num2 (parse-integer (coerce (first secondTriplet) 'string)))
    (num3 (parse-integer (coerce (first thirdTriplet) 'string)))
    (num4 (parse-integer (coerce (first fourhTriplet) 'string))))
    (cond 
      ((or (< num1 0) (> num1 255)
          (< num2 0) (> num2 255)
          (< num3 0) (> num3 255)
          (< num4 0) (> num4 255)) (error 'ip-non-valido))
      ((member (first (second fourhTriplet)) '(#\/ #\: eof)) 
      (values (make-instance 'host :value (coerce 
      (append (first firstTriplet) '(#\.) (first secondTriplet) '(#\.) (first thirdTriplet) '(#\.) (first fourhTriplet)) 'string)) (second fourhTriplet)))
      (T (error 'ip-non-valido)))))

(defun parse-host (list)
  (handler-case (parse-ip list)
      (error nil (parse-host-aux list))))

(defun parse-port (list)
   (if (not (eq (first list) #\:))
      (values 
        (make-instance 'port :value "80") 
        list)
      (multiple-value-bind (parsed remaining)
        (identificator (cdr list) '(#\/ eof))
        (cond ((null parsed) 
            (error 'uri-invalid-port))
          ((every #'digit-char-p (coerce parsed 'string)) (values 
              (make-instance 'port :value (coerce parsed 'string)) 
              remaining))
          (T (error 'uri-invalid-port))
        )
      )
  )
)

(defun parse-path-aux (list)
(cond 
  ((member (first list) '(#\? #\# eof)) (values nil list))
  (T (identificator-special list #\/ '(#\/ #\? #\# eof) '(#\: #\@)))))

(defun parse-path (list)
  (multiple-value-bind (parsed remaining)
    (parse-path-aux list)
    (cond ((null parsed) 
            (values nil remaining)
           )
          (T (values 
              (make-instance 'path :value (coerce parsed 'string)) 
              remaining))
     )
  )
)

(defun parse-id44 (list)
  "id44 è del tipo /<id44>(resto"
  (multiple-value-bind
   (parsed remaining)
   (identificator list '(#\( #\? #\# eof))
   (cond ((null parsed) (error 'uri-invalid-id44))
	 ((<= (list-length parsed) 44) (values (coerce parsed 'string) remaining)))
   )
  )

(defun parse-id8 (list)
  "id8 è del tipo (<id8>)"
    (multiple-value-bind
     (parsed remaining)
     (identificator list '(#\)))
     (cond ((null parsed) (error 'uri-invalid-id8))
	   ((<= (list-length parsed) 8) (values (coerce parsed 'string) remaining))       
	   )
     )
    )

  
(defun parse-path-zos (list)
   (if (not (eq (first list) #\/))
     (error 'uri-invalid-path-zos)
     (multiple-value-bind (id44 remaining)
     (parse-id44 (rest list))
     (cond ((null id44) 
	     (values nil remaining)) ; se parsed non è null allora devo recuperare se esiste id8 (controllo anche la ()
	   ((eq (first remaining) #\() ;se il primo carattere di rem è ( vuol dire che c'è id8
	      (multiple-value-bind (id8 extra)
				   (parse-id8 (cdr remaining))
				   (cond ((null id8) (error 'invalid-zos-path))
					        (T (values
					            (make-instance
						     'path :value (concatenate 'string id44 "(" id8 ")"))
						    (cdr extra)))
					)
				   ))
	    (T (values (make-instance 'path :value id44) remaining))))))

(defun parse-query (list)
  (if (not (eq (first list) #\?))
    (values nil list)
    (multiple-value-bind (parsed remaining)
      (identificator (cdr list) '(#\# eof))
      (cond ((null parsed) 
            (error 'uri-invalid-query)
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
            (error 'uri-invalid-fragment)
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

(defun slash (list)
  (cond
    ((or (eq (first list) 'eof) (null list)) list)
    ((eq (first list) #\/) (cdr list))
    (T (error 'uri-invalid-slash))
  )
)

; scheme ‘:’ authority [‘/’ [path] [‘?’ query] [‘#’ fragment]]
(defun parse-default-uri (URIStringList)
  (let* (
    (parsed-authority (multiple-value-list (parse-authority URIStringList)))
    (parsed-path (multiple-value-list (parse-path (slash (second parsed-authority)))))
    (parsed-query (multiple-value-list (parse-query (second parsed-path))))
    (parsed-fragment (multiple-value-list (parse-fragment (second parsed-query)))))
    (list 
      (first parsed-authority) 
      (first parsed-path) 
      (first parsed-query) 
      (first parsed-fragment)
      (second parsed-fragment)
    )
  )
)

(defun second-slash (list)
  (if (eq (first list) #\/) (cdr list)
    list))

; scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
; TODO Gestire il slash opzionale
(defun parse-resource-uri (URIStringList)
  (let* (
    (parsed-path (multiple-value-list (parse-path URIStringList)))
    (parsed-query (multiple-value-list (parse-query (second parsed-path))))
    (parsed-fragment (multiple-value-list (parse-fragment (second parsed-query)))))
    (list  
      (first parsed-path) 
      (first parsed-query) 
      (first parsed-fragment)
      (second parsed-fragment)
    )
  )
)

(defun parse-news (URIStringList)
  (let (
    (parsed-host (multiple-value-list (parse-host URIStringList))))
    (values 
      (make-uri-structure 
        (make-instance 'schema :value "news")
        (make-uri-authority 
          nil 
          (first parsed-host))) 
    (second parsed-host))
  )
)

(defun parse-telfax (URiStringList URIScheme)
(let (
    (parsed-userinfo (multiple-value-list (parse-userinfo URIStringList '(eof)))))
    (values 
      (make-uri-structure 
        (make-instance 'schema :value URIScheme)
        (make-uri-authority 
          (first parsed-userinfo))) 
    (second parsed-userinfo))
  )
)

(defun parse-mailto (URIStringList)
(let (
    (parsed-userinfo (multiple-value-list (parse-userinfo URIStringList '(eof)))))
    (if (equal (first (second parsed-userinfo)) #\@)
      (let (
        (parsed-host (multiple-value-list (parse-host (cdr (second parsed-userinfo))))))
        (values (make-uri-structure 
          (make-instance 'schema :value "mailto")
          (make-uri-authority 
            (first parsed-userinfo) 
            (first parsed-host)))  
        (second parsed-host)))
      (values(make-uri-structure 
          (make-instance 'schema :value "mailto")
          (make-uri-authority 
            (first parsed-userinfo)))   
      (second parsed-userinfo)))
  )
)

(defun parse-zos (URIStringList)
  (if (and (eq (first UriStringList) #\/) (eq (second UriStringList) #\/))
      (let* (
	 (parsed-authority (multiple-value-list (parse-authority (cdr (cdr URIStringList)))))
	 (parsed-path (multiple-value-list (parse-path-zos (second parsed-authority))))
	 (parsed-query (multiple-value-list (parse-query (second parsed-path))))
	 (parsed-fragment (multiple-value-list (parse-fragment (second parsed-query)))))
	  (values (make-uri-structure 
		   (make-instance 'schema :value "zos")
		   (first parsed-authority)
		   (first parsed-path)
		   (first parsed-query)
		   (first parsed-fragment)
		   )  
		  (second parsed-fragment)))
    (error 'uri-zos-invalid)))


(defun is-special-scheme (URIScheme)
  (or 
    (equalp URIScheme "news")
    (equalp URIScheme "zos")
    (equalp URIScheme "fax")
    (equalp URIScheme "tel")
    (equalp URIScheme "mailto")))

(defun parse-special-schema-uri (URIStringList URIScheme)
  (cond
    ((equalp URIScheme "news") (parse-news URIStringList))
    ((or (equalp URIScheme "fax") (equalp URIScheme "tel")) (parse-telfax URIStringList URIScheme))
    ((equalp URIScheme "mailto") (parse-mailto URIStringList))
    ((equalp URIScheme "zos") (parse-zos URIStringList))
  )
)

; Chiama la funzione corretta in base a cosa abbiamo dopo "scheme : "
(defun parse-uri-type (URIStringList)
  (let (
    (parsed-scheme (multiple-value-list (parse-scheme URIStringList))))
    (cond
      ((is-special-scheme (value (first parsed-scheme))) 
        (parse-special-schema-uri (second parsed-scheme) (value (first parsed-scheme)))
      )
      ((and
        (eq (first (second parsed-scheme)) #\/)
        (eq (second (second parsed-scheme)) #\/))
        (let 
          ((otherComp (parse-default-uri (cdr (cdr (second parsed-scheme))))))
          (values (make-instance 'uri-structure :schema (first parsed-scheme) 
                                        :authority (first otherComp)
                                        :path (second otherComp)
                                        :query (third otherComp)
                                        :fragment (fourth otherComp)) (fifth otherComp))))
      
      (T 
        (let 
          ((otherComp (parse-resource-uri (second-slash (second parsed-scheme)))))
          (values (make-instance 'uri-structure :schema (first parsed-scheme) 
                                        :authority (make-uri-authority)
                                        :path (first otherComp)
                                        :query (second otherComp)
                                        :fragment (third otherComp)) (fourth otherComp)))
      )
    )
  )
)

; Utility interna, aggiunge EOF alla fine della lista come delimitatore
; Richiama una funzione che gestisce i vari tipi di URI
(defun uri-parse_ (URIStringList)
  (let 
    ((parsed_uri (multiple-value-list (parse-uri-type (append URIStringList '(eof))))))
    (cond 
      ((not (eq (first (second parsed_uri)) 'eof)) (error 'invalid-uri))
      (T (first parsed_uri))
    )
  )
)

; Funzione principale, converta la stringa in Input come una lista di caratteri
(defun uri-parse (URIString)
  (handler-case (uri-parse_ (coerce URIString 'list))
    (error (c)
      (format t "URI non valido")
      (values nil c)
  ))
)
