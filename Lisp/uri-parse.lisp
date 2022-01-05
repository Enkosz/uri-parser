#| 866163 Avallone Lorenzo
   872783 Giannino Simone
   866147 Biotto Simone
|#
;Definizione delle Classi

; schema ::= <identificatore> 
(defclass schema ()
  ((value
    :initarg :value
    :accessor value)
   ))

;userinfo ::= <identificatore>
(defclass userinfo ()
  ((value
    :initarg :value
    :accessor value)
   ))

;host ::= <identificatore-host>
(defclass host ()
  ((value
    :initarg :value
    :accessor value)
   ))

;port ::= digit+
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

#| Funzioni d'accesso ai valori della struttura uri.
   Permetto di accedere a determinati valori della struttura, ritornando NIL
   in caso di assenza.
   Sono nel formato: (uri-<component-name> uri-parsed-struct)
   Se uri-parsed-struct è NIL i metodi ritorneranno NIL.
|#
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
; Funzione di supporto per la creazione di una istanza di authority
(defun make-uri-authority
	(&optional userinfo host (port (make-instance 'port :value 80)))
  (make-instance 'authority 
		 :userinfo userinfo
		 :host host
		 :port port))
; Funzione di supporto per la creazione di una istanza di uri-structure
(defun make-uri-structure
	(Scheme &optional (authority (make-uri-authority)) path query fragment)
  (make-instance 'uri-structure 
		 :schema Scheme
		 :authority authority
		 :path path
		 :query query
		 :fragment fragment))

#| Funzione che permette di recuperare tramite ricorsione identificatori delimi-
   tati dai caratteri inseriti in delimitators, di ritornare NIL in caso ci si
   imbatta nei caratteri inseriti in banned.
|#
(defun identificator% (list &optional delimitators banned accumulator)
  (when list
    (if (member (first list) delimitators)
	(values (nreverse accumulator) list)
      (when (not (member (first list) banned))
	(identificator% (rest list)
			delimitators
			banned
			(cons (car list) accumulator))))))
;Funzione di supporto per la gestione dei valori ritornati da identificator%
(defun identificator (list delimitator &optional banned)
  (let ((parse (multiple-value-list (identificator% list delimitator banned))))
    (if (first parse)
        (values-list parse)
      (values nil list))))
#| Funzione che permette di gestire i casi speciali di identificatore che sono
   ricorsivi su un carattere delimitatore (vedasi host).
|#
(defun identificator-special (list del delIdent &optional banned)
  (cond 
   ((member (first list) '(#\. #\/ #\? #\# #\@ #\: eof))
	(error 'uri-invalid-identificatorSpecial))
   (T (let ((parse (multiple-value-list (identificator list delIdent banned))))
		(if (eq (first (second parse)) del) ; abbiamo un subhost/subpath
			(let (
				  (secondParse
				   (multiple-value-list
					(identificator-special
					 (cdr (second parse)) del delIdent banned))))
			  (values
			   (append (first parse) (list del) (car secondParse))
			   (second secondParse)))  
		  (values-list parse))))))

(defun space-encoding (URIList)
  (cond 
   ((null URIList)
	nil)
   ((eq (car URIList) #\Space)
	(append '(#\% #\2 #\0) (space-encoding(cdr URIList))))
   (T
	(cons (car URIList) (space-encoding(cdr URIList))))))

(defun parse-scheme (list)
  (multiple-value-bind
   (parsed remaining)
   (identificator list '(#\:) (coerce "/?#@" 'list))
   (cond
	((null parsed) (error 'uri-invalid-scheme))
	(T (values
		(make-instance 'schema :value (coerce parsed 'string))
		(rest remaining))))))

(defun parse-userinfo (list &optional delimitatorSpecial)
  (multiple-value-bind
   (parsed remaining)
   (identificator
	list
	(append '(#\@) delimitatorSpecial)
	(set-difference '(#\/ #\? #\# #\: #\Space) delimitatorSpecial))
   (cond
	((null parsed) (values nil remaining))
	((not (null delimitatorSpecial))
	 (values
	  (make-instance 'userinfo :value (coerce parsed 'string)) 
	  remaining))
	(T (values
		(make-instance 'userinfo :value (coerce parsed 'string)) 
		(cdr remaining))))))

(defun parse-host-aux (list)
  (multiple-value-bind
   (parsed remaining)
   (identificator-special list #\. '(#\. #\/ #\: eof) '(#\? #\# #\@ #\Space))
   (cond
	((null parsed)
	 (error 'uri-invalid-host))
	(T (values 
		(make-instance 'host :value (coerce parsed 'string)) remaining)))))

(defun triplet (list &optional final)
  (let ((parse
		 (multiple-value-list
		  (identificator list '(#\. eof #\: #\/) '(#\@ #\? #\#  #\Space)))))
    (cond 
     ((null (first parse))
	  (error 'ip-non-valido))
     ((and final (eq (first (second parse)) #\.))
	  (error 'ip-non-valido))
     ((and (not final) (not(eq (first (second parse)) #\.)))
	  (error 'ip-non-valido))
     (T
	  (values (first parse) (second parse))))))

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
          (< num4 0) (> num4 255))
	  (error 'ip-non-valido))
     ((member (first (second fourhTriplet)) '(#\/ #\: eof)) 
      (values
	   (make-instance
		'host :value (coerce
					  (append (first firstTriplet)
							  '(#\.)
							  (first secondTriplet)
							  '(#\.)
							  (first thirdTriplet)
							  '(#\.)
							  (first fourhTriplet)) 'string))
	   (second fourhTriplet)))
     (T (error 'ip-non-valido)))))

(defun parse-host (list)
  (handler-case (parse-ip list)
    (error nil (parse-host-aux list))))

(defun parse-port (list)
  (if (not (eq (first list) #\:))
      (values 
       (make-instance 'port :value 80) 
       list)
    (multiple-value-bind
	 (parsed remaining)
	 (identificator (cdr list) '(#\/ eof))
	 (cond
	  ((null parsed) (error 'uri-invalid-port))
	  ((every #'digit-char-p (coerce parsed 'string))
	   (values
		(make-instance 'port :value (parse-integer (coerce parsed 'string)))
		remaining))
	  (T (error 'uri-invalid-port))))))

(defun parse-path-aux (list)
  (cond 
   ((member (first list) '(#\? #\# eof)) (values nil list))
   (T (identificator-special list #\/ '(#\/ #\? #\# eof) '(#\: #\@)))))

(defun parse-path (list)
  (multiple-value-bind
   (parsed remaining)
   (parse-path-aux list)
   (cond
	((null parsed) (values nil remaining))
	(T (values
		(make-instance 'path :value (coerce (space-encoding parsed) 'string)) 
		remaining)))))

(defun parse-id44 (list)
  (if (eq (first list) #\.) (error 'invalid-uri-id44)
    (multiple-value-bind
     (parsed remaining)
     (identificator list '(#\( #\? #\# eof) '(#\@ #\Space #\) #\%))
     (cond
	  ((null parsed) (error 'uri-invalid-id44))
	  ((and (<= (list-length parsed) 44) (not (eq (first (last parsed)) #\.)))
	   (values (coerce parsed 'string) remaining))))))

(defun parse-id8 (list)
  (multiple-value-bind
   (parsed remaining)
   (identificator list '(#\)) '(#\@ #\. #\Space #\? #\#))
   (cond
	((null parsed) (error 'uri-invalid-id8))
	((and
	  (<= (list-length parsed) 8)
	  (not (digit-char-p (first parsed)))
	  (every #'alphanumericp (coerce parsed 'string)))
	 (values (coerce parsed 'string) remaining)))))


(defun parse-path-zos (list)
  (cond 
    ;((eq (first list) 'eof) (values (make-instance 'path :value nil) list))
    ((not (eq (first list) #\/))
      (error 'uri-invalid-path-zos))
    ;((and (eq (first list) #\/)(eq (second list) 'eof)) (values (make-instance 'path :value nil) (cdr list)))
    (T (multiple-value-bind
     (id44 remaining)
     (parse-id44 (rest list))
     (cond ((null id44) 
	    (values nil remaining)) ; se parsed non è null allora devo recuperare se esiste id8 (controllo anche la ()
	   ((eq (first remaining) #\() ;se il primo carattere di rem è ( vuol dire che c'è id8
	    (multiple-value-bind
	     (id8 extra)
	     (parse-id8 (cdr remaining))
	     (cond ((null id8) (error 'invalid-zos-path))
		   (T (values
		       (make-instance
			'path :value (concatenate 'string id44 "(" id8 ")"))
		       (cdr extra))))))
	   (T (values (make-instance 'path :value id44) remaining)))))))

(defun parse-query (list)
  (if (not (eq (first list) #\?))
      (values nil list)
    (multiple-value-bind
     (parsed remaining)
     (identificator (cdr list) '(#\# eof))
     (cond ((null parsed) (error 'uri-invalid-query))
	   (T (values
	       (make-instance
			'userinfo :value (coerce (space-encoding parsed) 'string)) 
	       remaining))
	   ))))

(defun parse-fragment (list)
  (if (not (eq (first list) #\#))
      (values nil list)
    (multiple-value-bind
     (parsed remaining)
     (identificator (cdr list) '(eof))
     (cond ((null parsed) (error 'uri-invalid-fragment))
	   (T (values
	       (make-instance
			'userinfo :value (coerce (space-encoding parsed) 'string)) 
	        remaining))))))

; Restituisce un oggetto composed che contiene userinfo, host, port e il resto dell'input da parsare
(defun parse-authority (URIStringList)
  (if (and (eq (first URIStringList) #\/) 
           (eq (second URIStringList) #\/))
    (let* (
      (URIList (cdr (cdr URIStringList)))
      (parsed-userinfo (multiple-value-list
          (parse-userinfo URIList)))
      (parsed-host (multiple-value-list
            (parse-host (second parsed-userinfo))))
      (parsed-port (multiple-value-list
            (parse-port (second parsed-host)))))
      (values 
        (make-instance 'authority 
          :userinfo (first parsed-userinfo) 
          :host (first parsed-host) 
          :port (first parsed-port)) 
        (second parsed-port)))
    (values 
        (make-instance 'authority 
          :userinfo nil 
          :host nil 
          :port (make-instance 'port :value 80)) 
        URIStringList)))

(defun slash (list)
  (cond
   ((or (eq (first list) 'eof) (null list)) list)
   ((eq (first list) #\/) (cdr list))
   (T (error 'uri-invalid-slash))
   )
  )

; scheme ‘:’ [authority] [‘/’ [path] [‘?’ query] [‘#’ fragment]]
(defun parse-default-uri (URIStringList)
  (let* (
	 (parsed-authority (multiple-value-list
			    (parse-authority URIStringList)))
	 (parsed-path (multiple-value-list
		       (parse-path (slash (second parsed-authority)))))
	 (parsed-query (multiple-value-list
			(parse-query (second parsed-path))))
	 (parsed-fragment (multiple-value-list
			   (parse-fragment (second parsed-query)))))
    (list 
     (first parsed-authority) 
     (first parsed-path) 
     (first parsed-query) 
     (first parsed-fragment)
     (second parsed-fragment)
     )
    )
  )

(defun parse-news (URIStringList URIScheme)
    (let (
    (parsed-host (multiple-value-list (parse-host URIStringList))))
      (values 
      (make-uri-structure 
        (make-instance 'schema :value URIScheme)
        (make-uri-authority 
        nil 
        (first parsed-host))) 
      (second parsed-host))
    )
  )

(defun parse-telfax (URiStringList URIScheme)
  (let (
	(parsed-userinfo (multiple-value-list
			  (parse-userinfo URIStringList '(eof)))))
    (if (null (first parsed-userinfo))
      (error 'uri-invalid-telfax)
      (values 
      (make-uri-structure 
        (make-instance 'schema :value URIScheme)
        (make-uri-authority 
        (first parsed-userinfo))) 
      (second parsed-userinfo))
    )
  )
)

(defun parse-mailto (URIStringList URIScheme)
  (let (
	(parsed-userinfo (multiple-value-list
			  (parse-userinfo URIStringList '(eof)))))
    (if (equal (first (second parsed-userinfo)) #\@)
	    (let (
              (parsed-host (multiple-value-list
			    (parse-host (cdr (second parsed-userinfo))))))
        (if (null (first parsed-host))
          (error 'uri-invalid-mailto)
          (values (make-uri-structure 
		            (make-instance 'schema :value URIScheme)
		            (make-uri-authority 
		              (first parsed-userinfo) 
		              (first parsed-host)))  
		        (second parsed-host))))
      (if (null (first parsed-userinfo))
        (error 'uri-invalid-mailto)
        (values(make-uri-structure 
              (make-instance 'schema :value URIScheme)
              (make-uri-authority 
               (first parsed-userinfo)))   
	     (second parsed-userinfo))))
    )
  )

(defun parse-zos (URIStringList URIScheme)
      (let* (
	     (parsed-authority (multiple-value-list
				(parse-authority URIStringList)))
	     (parsed-path (multiple-value-list
			   (parse-path-zos (second parsed-authority))))
	     (parsed-query (multiple-value-list
			    (parse-query (second parsed-path))))
	     (parsed-fragment (multiple-value-list
			       (parse-fragment (second parsed-query)))))
	(values (make-uri-structure 
		 (make-instance 'schema :value URIScheme)
		 (first parsed-authority)
		 (first parsed-path)
		 (first parsed-query)
		 (first parsed-fragment)
		 )  
		(second parsed-fragment))))

(defun is-special-scheme (URIScheme)
  (or 
   (equalp (string-downcase URIScheme) "news")
   (equalp (string-downcase URIScheme) "zos")
   (equalp (string-downcase URIScheme) "fax")
   (equalp (string-downcase URIScheme) "tel")
   (equalp (string-downcase URIScheme) "mailto")))

(defun parse-special-schema-uri (URIStringList URIScheme)
  (cond
   ((and
    (eq (first UriStringList) 'eof)
    (equalp (string-downcase URIScheme) "zos"))
	(error 'uri-invalid-path-zos)) 
   ((eq (first UriStringList) 'eof)
	(values (make-uri-structure
			 (make-instance 'schema :value URIScheme)) URIStringList))
   ((equalp (string-downcase URIScheme) "news")
	(parse-news URIStringList URIScheme))
   ((or (equalp (string-downcase URIScheme) "fax")
		(equalp (string-downcase URIScheme) "tel"))
	(parse-telfax URIStringList URIScheme))
   ((equalp (string-downcase URIScheme) "mailto")
	(parse-mailto URIStringList URIScheme))
   ((equalp (string-downcase URIScheme) "zos")
	(parse-zos URIStringList URIScheme))
   )
  )

; Chiama la funzione corretta in base a cosa abbiamo dopo "scheme : "
(defun parse-uri-type (URIStringList)
  (let (
	(parsed-scheme (multiple-value-list (parse-scheme URIStringList))))
    (cond
     ((is-special-scheme (value (first parsed-scheme))) 
      (parse-special-schema-uri
       (second parsed-scheme) (value (first parsed-scheme)))
      )
     (T
      (let 
          ((otherComp (parse-default-uri (second parsed-scheme))))
        (values (make-instance 'uri-structure :schema (first parsed-scheme) 
                               :authority (first otherComp)
                               :path (second otherComp)
                               :query (third otherComp)
                               :fragment (fourth otherComp)) (fifth otherComp)))
      )
     )
    )
  )

; Utility interna, aggiunge EOF alla fine della lista come delimitatore
; Richiama una funzione che gestisce i vari tipi di URI
(defun uri-parse_ (URIStringList)
  (let 
      ((parsed_uri (multiple-value-list
					(parse-uri-type (append URIStringList '(eof))))))
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

(defun uri-display (URIStruct &optional (Stream t))
  (if (typep URIStruct 'uri-structure)
    (format Stream 
      (concatenate 'string
        "Scheme:   ~S~%" 
        "Userinfo: ~S~%"
        "Host:     ~S~%"
        "Port:     ~S~%"
        "Path:     ~S~%"
        "Query:    ~S~%"
        "Fragment: ~S~%") 
    (uri-scheme URIStruct) 
    (uri-userinfo URIStruct)
    (uri-host URIStruct)
    (uri-port URIStruct)
    (uri-path URIStruct)
    (uri-query URIStruct)
    (uri-fragment URIStruct))
    (format Stream "Uri invalid")
))

