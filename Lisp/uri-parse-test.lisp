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
    host 
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

; TEST HOST
(test "test-host-1"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-host-2"
    (uri-parse "scheme://userinfo@host")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-host-3"
    (uri-parse "scheme://host:123")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "123"
    )
)
(test "test-host-4"
    (uri-parse "scheme://userinfo@host:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port "123"
    )
)
(test "test-host-5"
    (uri-parse "scheme://host/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
    )
)
(test "test-host-6"
    (uri-parse "scheme://userinfo@host.com:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host.com"
        :port "123"
    )
)
(test "test-host_1"
    (uri-parse "scheme://userinfo@ho?st:123")
    nil
)
(test "test-host_2"
    (uri-parse "scheme://userinfo@ho@st:123")
    nil
)
(test "test-host_3"
    (uri-parse "scheme://userinfo@ho:st:123")
    nil
)
(test "test-host_4"
    (uri-parse "scheme://userinfo@ho/st:123")
    nil
)
(test "test-host_5"
    (uri-parse "scheme://userinfo@ho#st:123")
    nil
)
(test "test-host_6"
    (uri-parse "scheme://userinfo@ho st:123")
    nil
)
(test "test-host_7"
    (uri-parse "scheme://host..com")
    nil
)
(test "test-host_8"
    (uri-parse "scheme://host.")
    nil
)
(test "test-host_9"
    (uri-parse "scheme://host:")
    nil
)
(test "test-host_10"
    (uri-parse "scheme://host@")
    nil
)
(test "test-host_11"
    (uri-parse "scheme://host?")
    nil
)
(test "test-host_12"
    (uri-parse "scheme://host#")
    nil
)
(test "test-host_13"
    (uri-parse "scheme://host ")
    nil
)
(test "test-host_14"
    (uri-parse "scheme://.host")
    nil
)
(test "test-host_15"
    (uri-parse "scheme:///host")
    nil
)
(test "test-host_16"
    (uri-parse "scheme://@host")
    nil
)
(test "test-host_17"
    (uri-parse "scheme://:host")
    nil
)
(test "test-host_18"
    (uri-parse "scheme://?host")
    nil
)
(test "test-host_19"
    (uri-parse "scheme://#host")
    nil
)
(test "test-host_20"
    (uri-parse "scheme:// host")
    nil
)

;   TEST PORT
(test "test-port1"
    (uri-parse "scheme://host:123")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "123"
    )
)
(test "test-port2"
    (uri-parse "scheme://host:123/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "123"
        :path "path"
    )
)
(test "test-port3"
    (uri-parse "http://host:123")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
    )
)
(test "test-port4"
    (uri-parse "http://host")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
    )
)
(test "test-port5"
    (uri-parse "https://host:123")
    (prepare-uri
        :scheme "https"
        :host "host"
        :port "123"
    )
)
(test "test-port6"
    (uri-parse "https://host")
    (prepare-uri
        :scheme "https"
        :host "host"
        :port "80"
    )
)
(test "test-port_1"
    (uri-parse "scheme://host:1_23")
    nil
)
(test "test-port_2"
    (uri-parse "scheme://host:1a23")
    nil
)
(test "test-port_3"
    (uri-parse "scheme://host:")
    nil
)
(test "test-port_4"
    (uri-parse "scheme://host: ")
    nil
)

;   TEST PATH
(test "test-path1"
    (uri-parse "scheme://host/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
    )
)
(test "test-path2"
    (uri-parse "scheme://host/path/prova")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path/prova"
    )
)
(test "test-path3"
    (uri-parse "scheme://host/")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-path4"
    (uri-parse "scheme://host/path?query")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
    )
)
(test "test-path6"
    (uri-parse "scheme://host/pro.va")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "pro.va"
    )
)
(test "test-path_1"
    (uri-parse "scheme://host/path/")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/p:ath")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/p@ath")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host//path")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/@path")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/:path")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/path@")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/path:")
    nil
)
(test "test-path_1"
    (uri-parse "scheme://host/path//com")
    nil
)

; TEST QUERY
(test "test-query1"
    (uri-parse "scheme://host/?query")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "query"
    )
)
(test "test-query3"
    (uri-parse "scheme://host/?qu.ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu.ery"
    )
)
(test "test-query4"
    (uri-parse "scheme://host/?qu:ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu:ery"
    )
)
(test "test-query5"
    (uri-parse "scheme://host/?qu@ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu@ery"
    )
)
(test "test-query6"
    (uri-parse "scheme://host/?qu/ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu/ery"
    )
)
(test "test-query7"
    (uri-parse "scheme://host/?qu?ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu?ery"
    )
)
(test "test-query8"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-query9"
    (uri-parse "scheme://host/")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-query10"
    (uri-parse "scheme://host/path?query#fragment")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-query_1"
    (uri-parse "scheme://host/?")
    nil
)
(test "test-query_2"
    (uri-parse "scheme://host/?#")
    nil
)
(test "test-query_3"
    (uri-parse "scheme://host?query")
    nil
)

;   TEST FRAGMENT
(test "test-fragment1"
    (uri-parse "scheme://host/#frag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :fragment "frag"
    )
)
(test "test-fragment2"
    (uri-parse "scheme://host/path?query#frag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
        :fragment "frag"
    )
)
(test "test-fragment3"
    (uri-parse "scheme://host/?query#frag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "query"
        :fragment "frag"
    )
)
(test "test-fragment4"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-fragment_1"
    (uri-parse "scheme://host/#")
    nil
)
(test "test-fragment_2"
    (uri-parse "scheme://host#")
    nil
)

; TEST SCHEMA MAILTO
(test "test-mailto1"
    (uri-parse "mailto:userinfo")
    (prepare-uri
        :scheme "mailto"
        :userinfo "userinfo"
    )
)
(test "test-mailto2"
    (uri-parse "mailto:userinfo@host")
    (prepare-uri
        :scheme "mailto"
        :userinfo "userinfo"
        :host "host"
    )
)
(test "test-mailto_1"
    (uri-parse "mailto:")
    nil
)
(test "test-mailto_2"
    (uri-parse "mailto:userinfo@")
    nil
)
(test "test-mailto_3"
    (uri-parse "mailto:userinfo@host?query")
    nil
)
(test "test-mailto_4"
    (uri-parse "mailto:userinfo@host/path")
    nil
)

;   TEST SCHEMA FAX
(test "test-fax1"
    (uri-parse "fax:userinfo")
    (prepare-uri
        :scheme "fax"
        :userinfo "userinfo"
    )
)
(test "test-fax2"
    (uri-parse "fax:user123info")
    (prepare-uri
        :scheme "fax"
        :userinfo "user123info"
    )
)
(test "test-fax_1"
    (uri-parse "fax:")
    nil
)
(test "test-fax_2"
    (uri-parse "fax:user info")
    nil
)
(test "test-fax_3"
    (uri-parse "fax:userinfo@host")
    nil
)
(test "test-fax_4"
    (uri-parse "fax:userinfo/path")
    nil
)

;   TEST SCHEMA TEL
(test "test-tel1"
    (uri-parse "tel:userinfo")
    (prepare-uri
        :scheme "tel"
        :userinfo "userinfo"
    )
)
(test "test-tel2"
    (uri-parse "tel:user123info")
    (prepare-uri
        :scheme "tel"
        :userinfo "user123info"
    )
)
(test "test-tel3"
    (uri-parse "tel:0293564242")
    (prepare-uri
        :scheme "tel"
        :userinfo "0293564242"
    )
)
(test "test-tel_1"
    (uri-parse "tel:")
    nil
)
(test "test-tel_3"
    (uri-parse "tel:userinfo@host")
    nil
)
(test "test-tel_4"
    (uri-parse "tel:userinfo/path")
    nil
)

;   TEST SCHEMA NEWS
(test "test-news1"
    (uri-parse "news:host")
    (prepare-uri
        :scheme "news"
        :host "host"
    )
)
(test "test-news2"
    (uri-parse "news:host.subhost")
    (prepare-uri
        :scheme "news"
        :host "host.subhost"
    )
)
(test "test-news3"
    (uri-parse "news:ho123st")
    (prepare-uri
        :scheme "news"
        :host "ho123st"
    )
)
(test "test-news_1"
    (uri-parse "news:ho/st")
    nil
)
(test "test-news_1"
    (uri-parse "news:host/path")
    nil
)
(test "test-news_1"
    (uri-parse "news:host/path?query")
    nil
)
(test "test-news_1"
    (uri-parse "news:host/query?query#fragment")
    nil
)
(test "test-news_1"
    (uri-parse "news:host:80")
    nil
)
(test "test-news_1"
    (uri-parse "news:userinfo@host")
    nil
)










(format t "~%Run ~D tests: passed ~D, failed ~D" tot passed failed)