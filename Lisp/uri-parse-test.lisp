(load "./uri-parse.lisp")

(defparameter passed 0)
(defparameter failed 0)
(defparameter tot 0)

(defun test (name value expected) 
  (if (and (equalp (uri-scheme value) (uri-scheme expected))
    (equalp (uri-userinfo value) (uri-userinfo expected))
    (equalp (uri-host value) (uri-host expected))
    (equalp (uri-port value) (uri-port expected))
    (equalp (uri-path value) (uri-path expected))
    (equalp (uri-query value) (uri-query expected))
    (equalp (uri-fragment value) (uri-fragment expected)))
        (setq passed (1+  passed))
    (and (setq failed (1+ failed))
        (format T "~%Test ~A failed --> Expected: ~A Found: ~A" name expected value)))
  (setq tot (1+ tot))
)

(defun prepare-uri (
    &key
    (scheme (error "missing required arg scheme"))
    userinfo
    (host (error "missing required arg host"))
    port 
    path 
    query 
    fragment)
    (make-instance 'uri-structure 
        :schema (make-instance 'schema :value scheme)
        :authority  (make-instance 'authority 
            :userinfo (when userinfo (make-instance 'userinfo :value userinfo))
            :host (make-instance 'host :value host) 
            :port (when port (make-instance 'port :value port))
        )
        :path (when path (make-instance 'path :value path))
        :query (when query (make-instance 'query :value query))
        :fragment (when fragment (make-instance 'fragment :value fragment))
    )
)

; TEST SCHEMA
(test "test-scheme-1"
    (uri-parse "http://google.com")
    (prepare-uri 
        :scheme "http"
        :host "google.com"
        :port "80"
    )
)
(test "test-scheme-2"
    (uri-parse "h11ps://google.com")
    (prepare-uri 
        :scheme "h11ps"
        :host "google.com"
        :port "80"
    )
)
(test "test-scheme-3"
    (uri-parse "_http_://google.com")
    (prepare-uri 
        :scheme "_http_"
        :host "google.com"
        :port "80"
    )
)
(test "test-scheme_1"
    (uri-parse "://google.com")
    nil
)
(test "test-scheme_2"
    (uri-parse "@://google.com")
    nil
)
(test "test-scheme_3"
    (uri-parse "h:h://google.com")
    nil
)
(test "test-scheme_4"
    (uri-parse "/hh://google.com")
    nil
)
(test "test-scheme_5"
    (uri-parse "h#h://google.com")
    nil
)
(test "test-scheme_6"
    (uri-parse "h?h://google.com")
    nil
)
(test "test-scheme_7"
    (uri-parse ":://google.com")
    nil
)

; TEST USERINFO
(test "test-userinfo-1"
    (uri-parse "http://userinfo@host")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-userinfo-2"
    (uri-parse "http://user_info@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user_info"
        :host "host"
        :port "80"
    )
)
(test "test-userinfo-3"
    (uri-parse "http://user123info@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user123info"
        :host "host"
        :port "80"
    )
)
(test "test-userinfo_1"
    (uri-parse "http://user@info@host")
    nil
)
(test "test-userinfo_2"
    (uri-parse "http://userin:fo@host")
    nil
)
(test "test-userinfo_3"
    (uri-parse "http://userin/fo@host")
    nil
)
(test "test-userinfo_4"
    (uri-parse "http://userin?fo@host")
    nil
)
(test "test-userinfo_5"
    (uri-parse "http://userin#fo@host")
    nil
)
(test "test-userinfo_6"
    (uri-parse "http://userin fo@host")
    nil
)
(test "test-userinfo_7"
    (uri-parse "http://userinfo@")
    nil
)
(test "test-userinfo_8"
    (uri-parse "http://@")
    nil
)

(format t "~%Run ~D tests: passed ~D, failed ~D" tot passed failed)