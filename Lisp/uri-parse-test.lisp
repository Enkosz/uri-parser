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
(test "test-scheme1"
    (uri-parse "http://google.com")
    (prepare-uri 
        :scheme "http"
        :host "google.com"
        :port 80
    )
)
(test "test-scheme2"
    (uri-parse "h11ps://google.com")
    (prepare-uri 
        :scheme "h11ps"
        :host "google.com"
        :port 80
    )
)
(test "test-scheme3"
    (uri-parse "_http_://google.com")
    (prepare-uri 
        :scheme "_http_"
        :host "google.com"
        :port 80
    )
)
(test "test-scheme4"
    (uri-parse "s:")
    (prepare-uri 
        :scheme "s"
        :port 80
    )
)
(test "test-scheme5"
    (uri-parse "ss:")
    (prepare-uri 
        :scheme "ss"
        :port 80
    )
)
(test "test-scheme6"
    (uri-parse "s1:")
    (prepare-uri 
        :scheme "s1"
        :port 80
    )
)
(test "test-scheme7"
    (uri-parse "s+:")
    (prepare-uri 
        :scheme "s+"
        :port 80
    )
)
(test "test-scheme8"
    (uri-parse "s-:")
    (prepare-uri 
        :scheme "s-"
        :port 80
    )
)
(test "test-scheme9"
    (uri-parse "s.:")
    (prepare-uri 
        :scheme "s."
        :port 80
    )
)
(test "test-scheme10"
    (uri-parse "+:")
    (prepare-uri 
        :scheme "+"
        :port 80
    )
)
(test "test-scheme11"
    (uri-parse "1:")
    (prepare-uri 
        :scheme "1"
        :port 80
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
(test "test-scheme_8"
    (uri-parse "s/:")
    nil
)
(test "test-scheme_9"
    (uri-parse "s?:")
    nil
)
(test "test-scheme_10"
    (uri-parse "s#:")
    nil
)
(test "test-scheme_11"
    (uri-parse "s@:")
    nil
)
(test "test-scheme_12"
    (uri-parse "s::")
    nil
)
(test "test-scheme_13"
    (uri-parse ":")
    nil
)
#|(test "test-scheme_14"
    (uri-parse "ù:")
    nil
)|#




; TEST USERINFO
(test "test-userinfo1"
    (uri-parse "http://userinfo@host")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
    )
)
(test "test-userinfo2"
    (uri-parse "http://user_info@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user_info"
        :host "host"
        :port 80
    )
)
(test "test-userinfo3"
    (uri-parse "http://user123info@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user123info"
        :host "host"
        :port 80
    )
)
(test "test-userinfo4"
    (uri-parse "http://user.@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user."
        :host "host"
        :port 80
    )
)
(test "test-userinfo5"
    (uri-parse "s://u%20i@host")
    (prepare-uri
        :scheme "s"
        :userinfo "u%20i"
        :host "host"
        :port 80
    )
)
(test "test-userinfo6"
    (uri-parse "s://u-@host")
    (prepare-uri
        :scheme "s"
        :userinfo "u-"
        :host "host"
        :port 80
    )
)
(test "test-userinfo7"
    (uri-parse "s://u_@host")
    (prepare-uri
        :scheme "s"
        :userinfo "u_"
        :host "host"
        :port 80
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
(test "test-userinfo_8"
    (uri-parse "s://@host")
    nil
)

; TEST HOST
(test "test-host1"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-host2"
    (uri-parse "scheme://userinfo@host")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port 80
    )
)
(test "test-host3"
    (uri-parse "scheme://host:123")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 123
    )
)
(test "test-host4"
    (uri-parse "scheme://userinfo@host:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port 123
    )
)
(test "test-host5"
    (uri-parse "scheme://host/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "path"
    )
)
(test "test-host6"
    (uri-parse "scheme://userinfo@host.com:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host.com"
        :port 123
    )
)
(test "test-host7"
    (uri-parse "scheme://123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port 80
    )
)
(test "test-host8"
    (uri-parse "scheme://123.123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123.123"
        :port 80
    )
)
(test "test-host9"
    (uri-parse "scheme://123.123.123.1233")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.1233"
        :port 80
    )
)
(test "test-host10"
    (uri-parse "scheme://257.257.257.257")
    (prepare-uri
        :scheme "scheme"
        :host "257.257.257.257"
        :port 80
    )
)
(test "test-host11"
    (uri-parse "scheme://123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123"
        :port 80
    )
)
(test "test-host12"
    (uri-parse "scheme://userinfo@123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "123.123.123.123"
        :port 80
    )
)
(test "test-host13"
    (uri-parse "scheme://123.123.123.123:123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port 123
    )
)
(test "test-host14"
    (uri-parse "scheme://123.123.123.123/path")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port 80
        :path "path"
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
(test "test-host_21"
    (uri-parse "s://")
    nil
)

;   TEST PORT
(test "test-port1"
    (uri-parse "scheme://host:123")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 123
    )
)
(test "test-port2"
    (uri-parse "scheme://host:123/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 123
        :path "path"
    )
)
(test "test-port3"
    (uri-parse "http://host:123")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
    )
)
(test "test-port4"
    (uri-parse "http://host")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
    )
)
(test "test-port5"
    (uri-parse "https://host:123")
    (prepare-uri
        :scheme "https"
        :host "host"
        :port 123
    )
)
(test "test-port6"
    (uri-parse "https://host")
    (prepare-uri
        :scheme "https"
        :host "host"
        :port 80
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
(test "test-port_5"
    (uri-parse "s://:110")
    nil
)
(test "test-port_6"
    (uri-parse "s://host:a")
    nil
)

;   TEST PATH
(test "test-path1"
    (uri-parse "scheme://host/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "path"
    )
)
(test "test-path2"
    (uri-parse "scheme://host/path/prova")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "path/prova"
    )
)
(test "test-path3"
    (uri-parse "scheme://host/")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-path4"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-path5"
    (uri-parse "scheme://host/path?query")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test-path6"
    (uri-parse "scheme://host/pro va")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "pro%20va"
    )
)
(test "test-path7"
    (uri-parse "scheme://host/pro.va")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "pro.va"
    )
)
(test "test-path8"
    (uri-parse "scheme://host/path#fragment")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test-path_1"
    (uri-parse "scheme://host/path/")
    nil
)
(test "test-path_2"
    (uri-parse "scheme://host/p:ath")
    nil
)
(test "test-path_3"
    (uri-parse "scheme://host/p@ath")
    nil
)
(test "test-path_4"
    (uri-parse "scheme://host//path")
    nil
)
(test "test-path_5"
    (uri-parse "scheme://host/@path")
    nil
)
(test "test-path_6"
    (uri-parse "scheme://host/:path")
    nil
)
(test "test-path_7"
    (uri-parse "scheme://host/path@")
    nil
)
(test "test-path_8"
    (uri-parse "scheme://host/path:")
    nil
)
(test "test-path_9"
    (uri-parse "scheme://host/path//com")
    nil
)
(test "test-path_10"
    (uri-parse "scheme://host//path")
    nil
)

; TEST QUERY
(test "test-query1"
    (uri-parse "scheme://host/?query")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "query"
    )
)
(test "test-query1"
    (uri-parse "scheme://host/?qu ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu%20ery"
    )
)
(test "test-query3"
    (uri-parse "scheme://host/?qu.ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu.ery"
    )
)
(test "test-query4"
    (uri-parse "scheme://host/?qu:ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu:ery"
    )
)
(test "test-query5"
    (uri-parse "scheme://host/?qu@ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu@ery"
    )
)
(test "test-query6"
    (uri-parse "scheme://host/?qu/ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu/ery"
    )
)
(test "test-query7"
    (uri-parse "scheme://host/?qu?ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :query "qu?ery"
    )
)
(test "test-query8"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-query9"
    (uri-parse "scheme://host/")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-query10"
    (uri-parse "scheme://host/path?query#fragment")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
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
        :port 80
        :fragment "frag"
    )
)
(test "test-fragment2"
    (uri-parse "scheme://host/path?query#frag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
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
        :port 80
        :query "query"
        :fragment "frag"
    )
)
(test "test-fragment4"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
    )
)
(test "test-fragment5"
    (uri-parse "scheme://host/#fr ag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port 80
        :fragment "fr%20ag"
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
        :port 80
    )
)
(test "test-mailto2"
    (uri-parse "mailto:userinfo@host")
    (prepare-uri
        :scheme "mailto"
        :userinfo "userinfo"
        :host "host"
        :port 80
    )
)
(test "test-mailto3"
    (uri-parse "mailto:")
    (prepare-uri
        :scheme "mailto"
        :port 80
    )
)
(test "test-mailto4"
    (uri-parse "mailto:userinfo@host.com")
    (prepare-uri
        :scheme "mailto"
        :userinfo "userinfo"
        :host "host.com"
        :port 80
    )
)
(test "test-mailto5"
    (uri-parse "mailto:user.info")
    (prepare-uri
        :scheme "mailto"
        :userinfo "user.info"
        :port 80
    )
)
(test "test-mailto_1"
    (uri-parse "mailto:userinfo@")
    nil
)
(test "test-mailto_2"
    (uri-parse "mailto:userinfo@host?query")
    nil
)
(test "test-mailto_3"
    (uri-parse "mailto:userinfo@host/path")
    nil
)

;   TEST SCHEMA FAX
(test "test-fax1"
    (uri-parse "fax:userinfo")
    (prepare-uri
        :scheme "fax"
        :userinfo "userinfo"
        :port 80
    )
)
(test "test-fax2"
    (uri-parse "fax:user123info")
    (prepare-uri
        :scheme "fax"
        :userinfo "user123info"
        :port 80
    )
)
(test "test-fax3"
    (uri-parse "fax:")
    (prepare-uri
        :scheme "fax"
        :port 80
    )
)
(test "test-fax_1"
    (uri-parse "fax:user info")
    nil
)
(test "test-fax_2"
    (uri-parse "fax:userinfo@host")
    nil
)
(test "test-fax_3"
    (uri-parse "fax:userinfo/path")
    nil
)

;   TEST SCHEMA TEL
(test "test-tel1"
    (uri-parse "tel:userinfo")
    (prepare-uri
        :scheme "tel"
        :userinfo "userinfo"
        :port 80
    )
)
(test "test-tel2"
    (uri-parse "tel:user123info")
    (prepare-uri
        :scheme "tel"
        :userinfo "user123info"
        :port 80
    )
)
(test "test-tel3"
    (uri-parse "tel:0293564242")
    (prepare-uri
        :scheme "tel"
        :userinfo "0293564242"
        :port 80
    )
)
(test "test-tel4"
    (uri-parse "tel:")
    (prepare-uri
        :scheme "tel"
        :port 80
    )
)
(test "test-tel_1"
    (uri-parse "tel:user info")
    nil
)
(test "test-tel_2"
    (uri-parse "tel:userinfo@host")
    nil
)
(test "test-tel_3"
    (uri-parse "tel:userinfo/path")
    nil
)

;   TEST SCHEMA NEWS
(test "test-news1"
    (uri-parse "news:host")
    (prepare-uri
        :scheme "news"
        :host "host"
        :port 80
    )
)
(test "test-news2"
    (uri-parse "news:host.subhost")
    (prepare-uri
        :scheme "news"
        :host "host.subhost"
        :port 80
    )
)
(test "test-news3"
    (uri-parse "news:ho123st")
    (prepare-uri
        :scheme "news"
        :host "ho123st"
        :port 80
    )
)
(test "test-news4"
    (uri-parse "news:")
    (prepare-uri
        :scheme "news"
        :port 80    
    )
)
(test "test-news_1"
    (uri-parse "news:ho st")
    nil
)
(test "test-news_2"
    (uri-parse "news:ho/st")
    nil
)
(test "test-news_3"
    (uri-parse "news:host/path")
    nil
)
(test "test-news_4"
    (uri-parse "news:host/path?query")
    nil
)
(test "test-news_5"
    (uri-parse "news:host/query?query#fragment")
    nil
)
(test "test-news_6"
    (uri-parse "news:host:80")
    nil
)
(test "test-news_7"
    (uri-parse "news:userinfo@host")
    nil
)

;   TEST SCHEMA ZOS
(test "test-zos1"
    (uri-parse "zos://host/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44(id8)"
    )
)
(test "test-zos2"
    (uri-parse "zos://userinfo@host/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44(id8)"
    )
)
(test "test-zos3"
    (uri-parse "zos://userinfo@host:123/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44(id8)"
    )
)
(test "test-zos4"
    (uri-parse "zos://userinfo@host:123/id44(id8)?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44(id8)"
        :query "query"
    )
)
(test "test-zos5"
    (uri-parse "zos://userinfo@host:123/id44(id8)?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44(id8)"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-zos6"
    (uri-parse "zos://host/id.44(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id.44(id8)"
    )
)
(test "test-zos7"
    (uri-parse "zos://host/i.d.4.4(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "i.d.4.4(id8)"
    )
)
(test "test-zos8"
    (uri-parse "zos://host/i.d.4.4")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "i.d.4.4"
    )
)
(test "test-zos9"
    (uri-parse "zos://host/id..prova")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id..prova"
    )
)
(test "test-zos10"
    (uri-parse "zos://host/id..prova(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id..prova(id8)"
    )
)
(test "test-zos11"
    (uri-parse "zos://host/")
    nil
)
(test "test-zos12"
    (uri-parse "zos://host")
    nil
)
(test "test-zos13"
    (uri-parse "zos:/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44(id8)"
    )
)
(test "test-zos14"
    (uri-parse "zos:/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44(id8)"
    )
)
(test "test-zos15"
    (uri-parse "zos:/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44(id8)"
    )
)
(test "test-zos16"
    (uri-parse "zos:/id44(id8)?query")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44(id8)"
        :query "query"
    )
)
(test "test-zos17"
    (uri-parse "zos:/id44(id8)?query#fragment")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44(id8)"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-zos18"
    (uri-parse "zos:/id.44(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id.44(id8)"
    )
)
(test "test-zos19"
    (uri-parse "zos:/i.d.4.4(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "i.d.4.4(id8)"
    )
)
(test "test-zos20"
    (uri-parse "zos:/i.d.4.4")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "i.d.4.4"
    )
)
(test "test-zos21"
    (uri-parse "zos:/id..prova")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id..prova"
    )
)
(test "test-zos22"
    (uri-parse "zos:/id..prova(id8)")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id..prova(id8)"
    )
)
(test "test-zos23"
    (uri-parse "zos:/")
    nil
)
(test "test-zos24"
    (uri-parse "zos:")
    nil
)
(test "test-zos_1"
    (uri-parse "zos://host/.i.d")
    nil
)
(test "test-zos_2"
    (uri-parse "zos://host/.")
    nil
)
(test "test-zos_3"
    (uri-parse "zos://host/a012345678901234567890123456789012345678901234(id)")
    nil
)
(test "test-zos_4"
    (uri-parse "zos://host/path(a012345678)")
    nil
)
(test "test-zos_5"
    (uri-parse "zos://host/a012345678901234567890123456789012345678901234(a012345678)")
    nil
)
(test "test-zos_6"
    (uri-parse "zos://host/(id44)")
    nil
)
(test "test-zos_7"
    (uri-parse "zos://host/pi@ppo(id44)")
    nil
)
(test "test-zos_8"
    (uri-parse "zos://host/pip po(id44)")
    nil
)
(test "test-zos_9"
    (uri-parse "zos://host/pippo.mar co(id44)")
    nil
)
(test "test-zos_10"
    (uri-parse "zos://host/pippo(id.44)")
    nil
)
(test "test-zos_11"
    (uri-parse "zos://host/pippo(id .44)")
    nil
)
(test "test-zos_12"
    (uri-parse "zos://host/pippo()")
    nil
)
(test "test-zos_13"
    (uri-parse "zos://host/pippo( )")
    nil
)
(test "test-zos_14"
    (uri-parse "zos://host/pippo(")
    nil
)
(test "test-zos_15"
    (uri-parse "zos://host/pippo)")
    nil
)
(test "test-zos_16"
    (uri-parse "zos://host/id.")
    nil
)
(test "test-zos_17"
    (uri-parse "zos://host/id(1)")
    nil
)
(test "test-zos_18"
    (uri-parse "zos://host/id(1id8)")
    nil
)
(test "test-zos_19"
    (uri-parse "zos://host/id..prova..")
    nil
)
(test "test-zos_20"
    (uri-parse "zos://host/..id..prova")
    nil
)
(test "test-zos_21"
    (uri-parse "zos://host/id..prova..(id8)")
    nil
)
(test "test-zos_22"
    (uri-parse "zos://host/..id..prova(id8)")
    nil
)
(test "test-zos_23"
    (uri-parse "zos://host/.i.d(id8)")
    nil
)
(test "test-zos_24"
    (uri-parse "zos://host/.(id8)")
    nil
)
(test "test-zos_25"
    (uri-parse "zos://host/id.(id8)")
    nil
)
(test "test-zos_26"
    (uri-parse "zos:/.i.d")
    nil
)
(test "test-zos_27"
    (uri-parse "zos:/.")
    nil
)
(test "test-zos_28"
    (uri-parse "zos:/a012345678901234567890123456789012345678901234(id)")
    nil
)
(test "test-zos_29"
    (uri-parse "zos:/path(a012345678)")
    nil
)
(test "test-zos_30"
    (uri-parse "zos:/a012345678901234567890123456789012345678901234(a012345678)")
    nil
)
(test "test-zos_31"
    (uri-parse "zos:/(id44)")
    nil
)
(test "test-zos_32"
    (uri-parse "zos:/pi@ppo(id44)")
    nil
)
(test "test-zos_33"
    (uri-parse "zos:/pip po(id44)")
    nil
)
(test "test-zos_34"
    (uri-parse "zos:/pippo.mar co(id44)")
    nil
)
(test "test-zos_35"
    (uri-parse "zos:/pippo(id.44)")
    nil
)
(test "test-zos_36"
    (uri-parse "zos:/pippo(id .44)")
    nil
)
(test "test-zos_37"
    (uri-parse "zos:/pippo()")
    nil
)
(test "test-zos_38"
    (uri-parse "zos:/pippo( )")
    nil
)
(test "test-zos_39"
    (uri-parse "zos:/pippo(")
    nil
)
(test "test-zos_40"
    (uri-parse "zos:/pippo)")
    nil
)
(test "test-zos_41"
    (uri-parse "zos:/id.")
    nil
)
(test "test-zos_42"
    (uri-parse "zos:/id(1)")
    nil
)
(test "test-zos_43"
    (uri-parse "zos:/id(1id8)")
    nil
)
(test "test-zos_44"
    (uri-parse "zos:/id..prova..")
    nil
)
(test "test-zos_45"
    (uri-parse "zos:/..id..prova")
    nil
)
(test "test-zos_46"
    (uri-parse "zos:/id..prova..(id8)")
    nil
)
(test "test-zos_47"
    (uri-parse "zos:/..id..prova(id8)")
    nil
)
(test "test-zos_48"
    (uri-parse "zos:/.i.d(id8)")
    nil
)
(test "test-zos_49"
    (uri-parse "zos:/.(id8)")
    nil
)
(test "test-zos_50"
    (uri-parse "zos:/id.(id8)")
    nil
)


(test "test_uri_zos1"
    (uri-parse "zos://userinfo@host.subhost:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos2"
    (uri-parse "zos://userinfo@host.subhost:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos3"
    (uri-parse "zos://userinfo@host.subhost:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos4"
    (uri-parse "zos://userinfo@host.subhost:123/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos5"
    (uri-parse "zos://userinfo@host.subhost:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos6"
    (uri-parse "zos://userinfo@host.subhost:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos7"
    (uri-parse "zos://userinfo@host.subhost:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos8"
    (uri-parse "zos://userinfo@host.subhost:123/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos9"
    (uri-parse "zos://userinfo@host.subhost:123/?query#fragment")
    nil
)
(test "test_uri_zos10"
    (uri-parse "zos://userinfo@host.subhost:123/?query")
    nil
)

(test "test_uri_zos11"
    (uri-parse "zos://userinfo@host.subhost:123/#fragment")
    nil
)
(test "test_uri_zos12"
    (uri-parse "zos://userinfo@host.subhost:123/")
    nil
)
(test "test_uri_zos13"
    (uri-parse "zos://userinfo@host.subhost:123")
    nil
)
(test "test_uri_zos14"
    (uri-parse "zos://userinfo@host.subhost/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos15"
    (uri-parse "zos://userinfo@host.subhost/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos16"
    (uri-parse "zos://userinfo@host.subhost/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos17"
    (uri-parse "zos://userinfo@host.subhost/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos18"
    (uri-parse "zos://userinfo@host.subhost/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos19"
    (uri-parse "zos://userinfo@host.subhost/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos20"
    (uri-parse "zos://userinfo@host.subhost/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos21"
    (uri-parse "zos://userinfo@host.subhost/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos22"
    (uri-parse "zos://userinfo@host.subhost/?query#fragment")
    nil
)
(test "test_uri_zos23"
    (uri-parse "zos://userinfo@host.subhost/?query")
    nil
)

(test "test_uri_zos24"
    (uri-parse "zos://userinfo@host.subhost/#fragment")
    nil
)
(test "test_uri_zos25"
    (uri-parse "zos://userinfo@host.subhost/")
    nil
)
(test "test_uri_zos26"
    (uri-parse "zos://userinfo@host.subhost")
    nil
)
(test "test_uri_zos27"
    (uri-parse "zos://host.subhost:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos28"
    (uri-parse "zos://host.subhost:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos29"
    (uri-parse "zos://host.subhost:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos30"
    (uri-parse "zos://host.subhost:123/id44")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos31"
    (uri-parse "zos://host.subhost:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos32"
    (uri-parse "zos://host.subhost:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos33"
    (uri-parse "zos://host.subhost:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos34"
    (uri-parse "zos://host.subhost:123/id44")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos35"
    (uri-parse "zos://host.subhost:123/?query#fragment")
    nil
)
(test "test_uri_zos36"
    (uri-parse "zos://host.subhost:123/?query")
    nil
)

(test "test_uri_zos37"
    (uri-parse "zos://host.subhost:123/#fragment")
    nil
)
(test "test_uri_zos38"
    (uri-parse "zos://host.subhost:123/")
    nil
)
(test "test_uri_zos39"
    (uri-parse "zos://host.subhost:123")
    nil
)
(test "test_uri_zos40"
    (uri-parse "zos://host.subhost/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos41"
    (uri-parse "zos://host.subhost/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos42"
    (uri-parse "zos://host.subhost/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos43"
    (uri-parse "zos://host.subhost/id44")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos44"
    (uri-parse "zos://host.subhost/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos45"
    (uri-parse "zos://host.subhost/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos46"
    (uri-parse "zos://host.subhost/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos47"
    (uri-parse "zos://host.subhost/id44")
    (prepare-uri
        :scheme "zos"
        :host "host.subhost"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos48"
    (uri-parse "zos://host.subhost/?query#fragment")
    nil
)
(test "test_uri_zos49"
    (uri-parse "zos://host.subhost/?query")
    nil
)

(test "test_uri_zos50"
    (uri-parse "zos://host.subhost/#fragment")
    nil
)
(test "test_uri_zos51"
    (uri-parse "zos://host.subhost/")
    nil
)
(test "test_uri_zos52"
    (uri-parse "zos://host.subhost")
    nil
)




(test "test_uri_zos53"
    (uri-parse "zos://userinfo@host:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos54"
    (uri-parse "zos://userinfo@host:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos55"
    (uri-parse "zos://userinfo@host:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos56"
    (uri-parse "zos://userinfo@host:123/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos57"
    (uri-parse "zos://userinfo@host:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos58"
    (uri-parse "zos://userinfo@host:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos59"
    (uri-parse "zos://userinfo@host:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos60"
    (uri-parse "zos://userinfo@host:123/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos61"
    (uri-parse "zos://userinfo@host:123/?query#fragment")
    nil
)
(test "test_uri_zos62"
    (uri-parse "zos://userinfo@host:123/?query")
    nil
)

(test "test_uri_zos63"
    (uri-parse "zos://userinfo@host:123/#fragment")
    nil
)
(test "test_uri_zos64"
    (uri-parse "zos://userinfo@host:123/")
    nil
)
(test "test_uri_zos65"
    (uri-parse "zos://userinfo@host:123")
    nil
)
(test "test_uri_zos66"
    (uri-parse "zos://userinfo@host/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos67"
    (uri-parse "zos://userinfo@host/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos68"
    (uri-parse "zos://userinfo@host/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos69"
    (uri-parse "zos://userinfo@host/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos70"
    (uri-parse "zos://userinfo@host/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos71"
    (uri-parse "zos://userinfo@host/id44?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos72"
    (uri-parse "zos://userinfo@host/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos73"
    (uri-parse "zos://userinfo@host/id44")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos74"
    (uri-parse "zos://userinfo@host/?query#fragment")
    nil
)
(test "test_uri_zos75"
    (uri-parse "zos://userinfo@host/?query")
    nil
)

(test "test_uri_zos76"
    (uri-parse "zos://userinfo@host/#fragment")
    nil
)
(test "test_uri_zos77"
    (uri-parse "zos://userinfo@host/")
    nil
)
(test "test_uri_zos78"
    (uri-parse "zos://userinfo@host")
    nil
)
(test "test_uri_zos79"
    (uri-parse "zos://host:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos80"
    (uri-parse "zos://host:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos81"
    (uri-parse "zos://host:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos82"
    (uri-parse "zos://host:123/id44")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos83"
    (uri-parse "zos://host:123/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos84"
    (uri-parse "zos://host:123/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos85"
    (uri-parse "zos://host:123/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos86"
    (uri-parse "zos://host:123/id44")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 123
        :path "id44"
    )
)
(test "test_uri_zos87"
    (uri-parse "zos://host:123/?query#fragment")
    nil
)
(test "test_uri_zos88"
    (uri-parse "zos://host:123/?query")
    nil
)

(test "test_uri_zos89"
    (uri-parse "zos://host:123/#fragment")
    nil
)
(test "test_uri_zos90"
    (uri-parse "zos://host:123/")
    nil
)
(test "test_uri_zos91"
    (uri-parse "zos://host:123")
    nil
)
(test "test_uri_zos92"
    (uri-parse "zos://host/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos93"
    (uri-parse "zos://host/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos94"
    (uri-parse "zos://host/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos95"
    (uri-parse "zos://host/id44")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos96"
    (uri-parse "zos://host/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos97"
    (uri-parse "zos://host/id44?query")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos98"
    (uri-parse "zos://host/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos99"
    (uri-parse "zos://host/id44")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos100"
    (uri-parse "zos://host/?query#fragment")
    nil
)
(test "test_uri_zos101"
    (uri-parse "zos://host/?query")
    nil
)

(test "test_uri_zos102"
    (uri-parse "zos://host/#fragment")
    nil
)
(test "test_uri_zos103"
    (uri-parse "zos://host/")
    nil
)
(test "test_uri_zos104"
    (uri-parse "zos://host")
    nil
)
(test "test_uri_zos105"
    (uri-parse "zos:/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos106"
    (uri-parse "zos:/id44?query")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos107"
    (uri-parse "zos:/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos108"
    (uri-parse "zos:/id44")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos109"
    (uri-parse "zos:/id44?query#fragment")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri_zos110"
    (uri-parse "zos:/id44?query")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :query "query"
    )
)
(test "test_uri_zos111"
    (uri-parse "zos:/id44#fragment")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
        :fragment "fragment"
    )
)
(test "test_uri_zos112"
    (uri-parse "zos:/id44")
    (prepare-uri
        :scheme "zos"
        :port 80
        :path "id44"
    )
)
(test "test_uri_zos113"
    (uri-parse "zos:/?query#fragment")
    nil
)
(test "test_uri_zos114"
    (uri-parse "zos:/#fragment")
    nil
)
(test "test_uri_zos115"
    (uri-parse "zos:/?query")
    nil
)
(test "test_uri_zos116"
    (uri-parse "zos:/")
    nil
)
(test "test_uri_zos117"
    (uri-parse "zos:")
    nil
)

(test "test_uri_zos_1"
    (uri-parse "zos://")
    nil
)
(test "test_uri_zos_2"
    (uri-parse "zos:///id44?query#fragment")
    nil
)
(test "test_uri_zos_3"
    (uri-parse "zos:///id44?query")
    nil
)
(test "test_uri_zos_4"
    (uri-parse "zos:///id44#fragment")
    nil
)
(test "test_uri_zos_5"
    (uri-parse "zos:///id44")
    nil
)
(test "test_uri_zos_6"
    (uri-parse "zos:///id44?query#fragment")
    nil
)
(test "test_uri_zos_7"
    (uri-parse "zos:///id44?query")
    nil
)
(test "test_uri_zos_8"
    (uri-parse "zos:///id44#fragment")
    nil
)
(test "test_uri_zos_9"
    (uri-parse "zos:///id44")
    nil
)
(test "test_uri_zos_10"
    (uri-parse "zos:///?query#fragment")
    nil
)
(test "test_uri_zos_11"
    (uri-parse "zos:///?query")
    nil
)
(test "test_uri_zos_12"
    (uri-parse "zos:///#fragment")
    nil
)
(test "test_uri_zos_13"
    (uri-parse "zos:///")
    nil
)
(test "test_uri_zos_14"
    (uri-parse "zos://")
    nil
)
(test "test_uri_zos_15"
    (uri-parse "zos://host?query#fragment")
    nil
)
(test "test_uri_zos_16"
    (uri-parse "zos://host?query")
    nil
)
(test "test_uri_zos_17"
    (uri-parse "zos://host#fragment")
    nil
)
(test "test_uri_zos_18"
    (uri-parse "://host/id44?query#fragment")
    nil
)
(test "test_uri_zos_19"
    (uri-parse "://host/id44?query")
    nil
)
(test "test_uri_zos_20"
    (uri-parse "://host/id44#fragment")
    nil
)
(test "test_uri_zos_21"
    (uri-parse "://host/id44")
    nil
)
(test "test_uri_zos_22"
    (uri-parse "://host/id44?query#fragment")
    nil
)
(test "test_uri_zos_23"
    (uri-parse "://host/id44?query")
    nil
)
(test "test_uri_zos_24"
    (uri-parse "://host/id44#fragment")
    nil
)
(test "test_uri_zos_25"
    (uri-parse "://host/id44")
    nil
)
(test "test_uri_zos_26"
    (uri-parse "://host/?query#fragment")
    nil
)
(test "test_uri_zos_27"
    (uri-parse "://host/?query")
    nil
)
(test "test_uri_zos_28"
    (uri-parse "://host/#fragment")
    nil
)
(test "test_uri_zos_29"
    (uri-parse "://host/")
    nil
)
(test "test_uri_zos_30"
    (uri-parse "://host")
    nil
)
(test "test_uri_zos_31"
    (uri-parse "zos//host/id44?query#fragment")
    nil
)
(test "test_uri_zos_32"
    (uri-parse "zos//host/id44?query")
    nil
)
(test "test_uri_zos_33"
    (uri-parse "zos//host/id44#fragment")
    nil
)
(test "test_uri_zos_34"
    (uri-parse "zos//host/id44")
    nil
)
(test "test_uri_zos_35"
    (uri-parse "zos//host/id44?query#fragment")
    nil
)
(test "test_uri_zos_36"
    (uri-parse "zos//host/id44?query")
    nil
)
(test "test_uri_zos_37"
    (uri-parse "zos//host/id44#fragment")
    nil
)
(test "test_uri_zos_38"
    (uri-parse "zos//host/id44")
    nil
)
(test "test_uri_zos_39"
    (uri-parse "zos//host/?query#fragment")
    nil
)
(test "test_uri_zos_40"
    (uri-parse "zos//host/?query")
    nil
)
(test "test_uri_zos_41"
    (uri-parse "zos//host/#fragment")
    nil
)
(test "test_uri_zos_42"
    (uri-parse "zos//host/")
    nil
)
(test "test_uri_zos_43"
    (uri-parse "zos//host")
    nil
)
(test "test_uri_zos_44"
    (uri-parse "zos:path?query#fragment")
    nil
)
(test "test_uri_zos_45"
    (uri-parse "zos:path?query")
    nil
)
(test "test_uri_zos_46"
    (uri-parse "zos:path#fragment")
    nil
)
(test "test_uri_zos_47"
    (uri-parse "zos:path")
    nil
)
(test "test_uri_zos_48"
    (uri-parse "zos:path?query#fragment")
    nil
)
(test "test_uri_zos_49"
    (uri-parse "zos:path?query")
    nil
)
(test "test_uri_zos_50"
    (uri-parse "zos:path#fragment")
    nil
)
(test "test_uri_zos_51"
    (uri-parse "zos:path")
    nil
)
(test "test_uri_zos_52"
    (uri-parse "zos:?query#fragment")
    nil
)
(test "test_uri_zos_53"
    (uri-parse "zos:#fragment")
    nil
)
(test "test_uri_zos_54"
    (uri-parse "zos:?query")
    nil
)


(test "test_uri_zos_55"
    (uri-parse "zos/id44?query#fragment")
    nil
)
(test "test_uri_zos_56"
    (uri-parse "zos/id44?query")
    nil
)
(test "test_uri_zos_57"
    (uri-parse "zos/id44#fragment")
    nil
)
(test "test_uri_zos_58"
    (uri-parse "zos/id44")
    nil
)
(test "test_uri_zos_59"
    (uri-parse "zos/id44?query#fragment")
    nil
)
(test "test_uri_zos_60"
    (uri-parse "zos/id44?query")
    nil
)
(test "test_uri_zos_61"
    (uri-parse "zos/id44#fragment")
    nil
)
(test "test_uri_zos_62"
    (uri-parse "zos/id44")
    nil
)
(test "test_uri_zos_63"
    (uri-parse "zos/?query#fragment")
    nil
)
(test "test_uri_zos_64"
    (uri-parse "zos/#fragment")
    nil
)
(test "test_uri_zos_65"
    (uri-parse "zos/?query")
    nil
)
(test "test_uri_zos_66"
    (uri-parse "zos/")
    nil
)
(test "test_uri_zos_67"
    (uri-parse "zospath?query#fragment")
    nil
)
(test "test_uri_zos_68"
    (uri-parse "zospath?query")
    nil
)
(test "test_uri_zos_69"
    (uri-parse "zospath#fragment")
    nil
)
(test "test_uri_zos_70"
    (uri-parse "zospath")
    nil
)
(test "test_uri_zos_71"
    (uri-parse "zospath?query#fragment")
    nil
)
(test "test_uri_zos_72"
    (uri-parse "zospath?query")
    nil
)
(test "test_uri_zos_73"
    (uri-parse "zospath#fragment")
    nil
)
(test "test_uri_zos_74"
    (uri-parse "zospath")
    nil
)
(test "test_uri_zos_75"
    (uri-parse "zos?query#fragment")
    nil
)
(test "test_uri_zos_76"
    (uri-parse "zos#fragment")
    nil
)
(test "test_uri_zos_77"
    (uri-parse "zos?query")
    nil
)
(test "test_uri_zos_78"
    (uri-parse "zos")
    nil
)
(test "test_uri_zos_79"
    (uri-parse ":/id44?query#fragment")
    nil
)
(test "test_uri_zos_80"
    (uri-parse ":/id44?query")
    nil
)
(test "test_uri_zos_81"
    (uri-parse ":/id44#fragment")
    nil
)
(test "test_uri_zos_82"
    (uri-parse ":/id44")
    nil
)
(test "test_uri_zos_83"
    (uri-parse ":/id44?query#fragment")
    nil
)
(test "test_uri_zos_84"
    (uri-parse ":/id44?query")
    nil
)
(test "test_uri_zos_85"
    (uri-parse ":/id44#fragment")
    nil
)
(test "test_uri_zos_86"
    (uri-parse ":/id44")
    nil
)
(test "test_uri_zos_87"
    (uri-parse ":/?query#fragment")
    nil
)
(test "test_uri_zos_88"
    (uri-parse ":/#fragment")
    nil
)
(test "test_uri_zos_89"
    (uri-parse ":/?query")
    nil
)
(test "test_uri_zos_90"
    (uri-parse ":/")
    nil
)
(test "test_uri_zos_91"
    (uri-parse ":path?query#fragment")
    nil
)
(test "test_uri_zos_92"
    (uri-parse ":path?query")
    nil
)
(test "test_uri_zos_93"
    (uri-parse ":path#fragment")
    nil
)
(test "test_uri_zos_94"
    (uri-parse ":path")
    nil
)
(test "test_uri_zos_95"
    (uri-parse ":path?query#fragment")
    nil
)
(test "test_uri_zos_96"
    (uri-parse ":path?query")
    nil
)
(test "test_uri_zos_97"
    (uri-parse ":path#fragment")
    nil
)
(test "test_uri_zos_98"
    (uri-parse ":path")
    nil
)
(test "test_uri_zos_99"
    (uri-parse ":?query#fragment")
    nil
)
(test "test_uri_zos_100"
    (uri-parse ":#fragment")
    nil
)
(test "test_uri_zos_101"
    (uri-parse ":?query")
    nil
)
(test "test_uri_zos_102"
    (uri-parse ":")
    nil
)


; TEST INTEGRATION ---- PRIMO TIPO DI URI
(test "test_uri1"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri2"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri3"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri4"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
    )
)
(test "test_uri5"
    (uri-parse "http://userinfo@host.subhost:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri6"
    (uri-parse "http://userinfo@host.subhost:123/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path"
        :query "query"
    )
)
(test "test_uri7"
    (uri-parse "http://userinfo@host.subhost:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri8"
    (uri-parse "http://userinfo@host.subhost:123/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :path "path"
    )
)
(test "test_uri9"
    (uri-parse "http://userinfo@host.subhost:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri10"
    (uri-parse "http://userinfo@host.subhost:123/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :query "query"
    )
)

(test "test_uri11"
    (uri-parse "http://userinfo@host.subhost:123/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
        :fragment "fragment"
    )
)
(test "test_uri12"
    (uri-parse "http://userinfo@host.subhost:123/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
    )
)
(test "test_uri13"
    (uri-parse "http://userinfo@host.subhost:123")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 123
    )
)
(test "test_uri14"
    (uri-parse "http://userinfo@host.subhost/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri15"
    (uri-parse "http://userinfo@host.subhost/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri16"
    (uri-parse "http://userinfo@host.subhost/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri17"
    (uri-parse "http://userinfo@host.subhost/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
    )
)
(test "test_uri18"
    (uri-parse "http://userinfo@host.subhost/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri19"
    (uri-parse "http://userinfo@host.subhost/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test_uri20"
    (uri-parse "http://userinfo@host.subhost/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri21"
    (uri-parse "http://userinfo@host.subhost/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :path "path"
    )
)
(test "test_uri22"
    (uri-parse "http://userinfo@host.subhost/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri23"
    (uri-parse "http://userinfo@host.subhost/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :query "query"
    )
)

(test "test_uri24"
    (uri-parse "http://userinfo@host.subhost/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
        :fragment "fragment"
    )
)
(test "test_uri25"
    (uri-parse "http://userinfo@host.subhost/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
    )
)
(test "test_uri26"
    (uri-parse "http://userinfo@host.subhost")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port 80
    )
)
(test "test_uri27"
    (uri-parse "http://host.subhost:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri28"
    (uri-parse "http://host.subhost:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri29"
    (uri-parse "http://host.subhost:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri30"
    (uri-parse "http://host.subhost:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path/subpath"
    )
)
(test "test_uri31"
    (uri-parse "http://host.subhost:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri32"
    (uri-parse "http://host.subhost:123/path?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path"
        :query "query"
    )
)
(test "test_uri33"
    (uri-parse "http://host.subhost:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri34"
    (uri-parse "http://host.subhost:123/path")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :path "path"
    )
)
(test "test_uri35"
    (uri-parse "http://host.subhost:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri36"
    (uri-parse "http://host.subhost:123/?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :query "query"
    )
)

(test "test_uri37"
    (uri-parse "http://host.subhost:123/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
        :fragment "fragment"
    )
)
(test "test_uri38"
    (uri-parse "http://host.subhost:123/")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
    )
)
(test "test_uri39"
    (uri-parse "http://host.subhost:123")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 123
    )
)
(test "test_uri40"
    (uri-parse "http://host.subhost/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri41"
    (uri-parse "http://host.subhost/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri42"
    (uri-parse "http://host.subhost/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri43"
    (uri-parse "http://host.subhost/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path/subpath"
    )
)
(test "test_uri44"
    (uri-parse "http://host.subhost/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri45"
    (uri-parse "http://host.subhost/path?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test_uri46"
    (uri-parse "http://host.subhost/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri47"
    (uri-parse "http://host.subhost/path")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :path "path"
    )
)
(test "test_uri48"
    (uri-parse "http://host.subhost/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri49"
    (uri-parse "http://host.subhost/?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :query "query"
    )
)

(test "test_uri50"
    (uri-parse "http://host.subhost/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
        :fragment "fragment"
    )
)
(test "test_uri51"
    (uri-parse "http://host.subhost/")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
    )
)
(test "test_uri52"
    (uri-parse "http://host.subhost")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port 80
    )
)




(test "test_uri53"
    (uri-parse "http://userinfo@host:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri54"
    (uri-parse "http://userinfo@host:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri55"
    (uri-parse "http://userinfo@host:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri56"
    (uri-parse "http://userinfo@host:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path/subpath"
    )
)
(test "test_uri57"
    (uri-parse "http://userinfo@host:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri58"
    (uri-parse "http://userinfo@host:123/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path"
        :query "query"
    )
)
(test "test_uri59"
    (uri-parse "http://userinfo@host:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri60"
    (uri-parse "http://userinfo@host:123/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :path "path"
    )
)
(test "test_uri61"
    (uri-parse "http://userinfo@host:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri62"
    (uri-parse "http://userinfo@host:123/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :query "query"
    )
)

(test "test_uri63"
    (uri-parse "http://userinfo@host:123/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
        :fragment "fragment"
    )
)
(test "test_uri64"
    (uri-parse "http://userinfo@host:123/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
    )
)
(test "test_uri65"
    (uri-parse "http://userinfo@host:123")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 123
    )
)
(test "test_uri66"
    (uri-parse "http://userinfo@host/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri67"
    (uri-parse "http://userinfo@host/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri68"
    (uri-parse "http://userinfo@host/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri69"
    (uri-parse "http://userinfo@host/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path/subpath"
    )
)
(test "test_uri70"
    (uri-parse "http://userinfo@host/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri71"
    (uri-parse "http://userinfo@host/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test_uri72"
    (uri-parse "http://userinfo@host/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri73"
    (uri-parse "http://userinfo@host/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :path "path"
    )
)
(test "test_uri74"
    (uri-parse "http://userinfo@host/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri75"
    (uri-parse "http://userinfo@host/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :query "query"
    )
)

(test "test_uri76"
    (uri-parse "http://userinfo@host/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
        :fragment "fragment"
    )
)
(test "test_uri77"
    (uri-parse "http://userinfo@host/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
    )
)
(test "test_uri78"
    (uri-parse "http://userinfo@host")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port 80
    )
)
(test "test_uri79"
    (uri-parse "http://host:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri80"
    (uri-parse "http://host:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri81"
    (uri-parse "http://host:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri82"
    (uri-parse "http://host:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path/subpath"
    )
)
(test "test_uri83"
    (uri-parse "http://host:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri84"
    (uri-parse "http://host:123/path?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path"
        :query "query"
    )
)
(test "test_uri85"
    (uri-parse "http://host:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri86"
    (uri-parse "http://host:123/path")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :path "path"
    )
)
(test "test_uri87"
    (uri-parse "http://host:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri88"
    (uri-parse "http://host:123/?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :query "query"
    )
)

(test "test_uri89"
    (uri-parse "http://host:123/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
        :fragment "fragment"
    )
)
(test "test_uri90"
    (uri-parse "http://host:123/")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
    )
)
(test "test_uri91"
    (uri-parse "http://host:123")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 123
    )
)
(test "test_uri92"
    (uri-parse "http://host/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri93"
    (uri-parse "http://host/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri94"
    (uri-parse "http://host/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri95"
    (uri-parse "http://host/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path/subpath"
    )
)
(test "test_uri96"
    (uri-parse "http://host/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri97"
    (uri-parse "http://host/path?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test_uri98"
    (uri-parse "http://host/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri99"
    (uri-parse "http://host/path")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :path "path"
    )
)
(test "test_uri100"
    (uri-parse "http://host/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri101"
    (uri-parse "http://host/?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :query "query"
    )
)

(test "test_uri102"
    (uri-parse "http://host/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
        :fragment "fragment"
    )
)
(test "test_uri103"
    (uri-parse "http://host/")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
    )
)
(test "test_uri104"
    (uri-parse "http://host")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port 80
    )
)
(test "test_uri105"
    (uri-parse "http:/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri106"
    (uri-parse "http:/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path/subpath"
        :query "query"
    )
)
(test "test_uri107"
    (uri-parse "http:/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test_uri108"
    (uri-parse "http:/path/subpath")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path/subpath"
    )
)
(test "test_uri109"
    (uri-parse "http:/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri110"
    (uri-parse "http:/path?query")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path"
        :query "query"
    )
)
(test "test_uri111"
    (uri-parse "http:/path#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path"
        :fragment "fragment"
    )
)
(test "test_uri112"
    (uri-parse "http:/path")
    (prepare-uri
        :scheme "http"
        :port 80
        :path "path"
    )
)
(test "test_uri113"
    (uri-parse "http:/?query#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :query "query"
        :fragment "fragment"
    )
)
(test "test_uri114"
    (uri-parse "http:/#fragment")
    (prepare-uri
        :scheme "http"
        :port 80
        :fragment "fragment"
    )
)
(test "test_uri115"
    (uri-parse "http:/?query")
    (prepare-uri
        :scheme "http"
        :port 80
        :query "query"
    )
)
(test "test_uri116"
    (uri-parse "http:/")
    (prepare-uri
        :scheme "http"
        :port 80
    )
)
(test "test_uri117"
    (uri-parse "http:")
    (prepare-uri
        :scheme "http"
        :port 80
    )
)

(test "test_uri_1"
    (uri-parse "http://")
    nil
)
(test "test_uri_2"
    (uri-parse "http:///path/subpath?query#fragment")
    nil
)
(test "test_uri_3"
    (uri-parse "http:///path/subpath?query")
    nil
)
(test "test_uri_4"
    (uri-parse "http:///path/subpath#fragment")
    nil
)
(test "test_uri_5"
    (uri-parse "http:///path/subpath")
    nil
)
(test "test_uri_6"
    (uri-parse "http:///path?query#fragment")
    nil
)
(test "test_uri_7"
    (uri-parse "http:///path?query")
    nil
)
(test "test_uri_8"
    (uri-parse "http:///path#fragment")
    nil
)
(test "test_uri_9"
    (uri-parse "http:///path")
    nil
)
(test "test_uri_10"
    (uri-parse "http:///?query#fragment")
    nil
)
(test "test_uri_11"
    (uri-parse "http:///?query")
    nil
)
(test "test_uri_12"
    (uri-parse "http:///#fragment")
    nil
)
(test "test_uri_13"
    (uri-parse "http:///")
    nil
)
(test "test_uri_14"
    (uri-parse "http://")
    nil
)
(test "test_uri_15"
    (uri-parse "http://host?query#fragment")
    nil
)
(test "test_uri_16"
    (uri-parse "http://host?query")
    nil
)
(test "test_uri_17"
    (uri-parse "http://host#fragment")
    nil
)
(test "test_uri_18"
    (uri-parse "://host/path/subpath?query#fragment")
    nil
)
(test "test_uri_19"
    (uri-parse "://host/path/subpath?query")
    nil
)
(test "test_uri_20"
    (uri-parse "://host/path/subpath#fragment")
    nil
)
(test "test_uri_21"
    (uri-parse "://host/path/subpath")
    nil
)
(test "test_uri_22"
    (uri-parse "://host/path?query#fragment")
    nil
)
(test "test_uri_23"
    (uri-parse "://host/path?query")
    nil
)
(test "test_uri_24"
    (uri-parse "://host/path#fragment")
    nil
)
(test "test_uri_25"
    (uri-parse "://host/path")
    nil
)
(test "test_uri_26"
    (uri-parse "://host/?query#fragment")
    nil
)
(test "test_uri_27"
    (uri-parse "://host/?query")
    nil
)
(test "test_uri_28"
    (uri-parse "://host/#fragment")
    nil
)
(test "test_uri_29"
    (uri-parse "://host/")
    nil
)
(test "test_uri_30"
    (uri-parse "://host")
    nil
)
(test "test_uri_31"
    (uri-parse "http//host/path/subpath?query#fragment")
    nil
)
(test "test_uri_32"
    (uri-parse "http//host/path/subpath?query")
    nil
)
(test "test_uri_33"
    (uri-parse "http//host/path/subpath#fragment")
    nil
)
(test "test_uri_34"
    (uri-parse "http//host/path/subpath")
    nil
)
(test "test_uri_35"
    (uri-parse "http//host/path?query#fragment")
    nil
)
(test "test_uri_36"
    (uri-parse "http//host/path?query")
    nil
)
(test "test_uri_37"
    (uri-parse "http//host/path#fragment")
    nil
)
(test "test_uri_38"
    (uri-parse "http//host/path")
    nil
)
(test "test_uri_39"
    (uri-parse "http//host/?query#fragment")
    nil
)
(test "test_uri_40"
    (uri-parse "http//host/?query")
    nil
)
(test "test_uri_41"
    (uri-parse "http//host/#fragment")
    nil
)
(test "test_uri_42"
    (uri-parse "http//host/")
    nil
)
(test "test_uri_43"
    (uri-parse "http//host")
    nil
)
(test "test_uri_44"
    (uri-parse "http:path/subpath?query#fragment")
    nil
)
(test "test_uri_45"
    (uri-parse "http:path/subpath?query")
    nil
)
(test "test_uri_46"
    (uri-parse "http:path/subpath#fragment")
    nil
)
(test "test_uri_47"
    (uri-parse "http:path/subpath")
    nil
)
(test "test_uri_48"
    (uri-parse "http:path?query#fragment")
    nil
)
(test "test_uri_49"
    (uri-parse "http:path?query")
    nil
)
(test "test_uri_50"
    (uri-parse "http:path#fragment")
    nil
)
(test "test_uri_51"
    (uri-parse "http:path")
    nil
)
(test "test_uri_52"
    (uri-parse "http:?query#fragment")
    nil
)
(test "test_uri_53"
    (uri-parse "http:#fragment")
    nil
)
(test "test_uri_54"
    (uri-parse "http:?query")
    nil
)


(test "test_uri_55"
    (uri-parse "http/path/subpath?query#fragment")
    nil
)
(test "test_uri_56"
    (uri-parse "http/path/subpath?query")
    nil
)
(test "test_uri_57"
    (uri-parse "http/path/subpath#fragment")
    nil
)
(test "test_uri_58"
    (uri-parse "http/path/subpath")
    nil
)
(test "test_uri_59"
    (uri-parse "http/path?query#fragment")
    nil
)
(test "test_uri_60"
    (uri-parse "http/path?query")
    nil
)
(test "test_uri_61"
    (uri-parse "http/path#fragment")
    nil
)
(test "test_uri_62"
    (uri-parse "http/path")
    nil
)
(test "test_uri_63"
    (uri-parse "http/?query#fragment")
    nil
)
(test "test_uri_64"
    (uri-parse "http/#fragment")
    nil
)
(test "test_uri_65"
    (uri-parse "http/?query")
    nil
)
(test "test_uri_66"
    (uri-parse "http/")
    nil
)
(test "test_uri_67"
    (uri-parse "httppath/subpath?query#fragment")
    nil
)
(test "test_uri_68"
    (uri-parse "httppath/subpath?query")
    nil
)
(test "test_uri_69"
    (uri-parse "httppath/subpath#fragment")
    nil
)
(test "test_uri_70"
    (uri-parse "httppath/subpath")
    nil
)
(test "test_uri_71"
    (uri-parse "httppath?query#fragment")
    nil
)
(test "test_uri_72"
    (uri-parse "httppath?query")
    nil
)
(test "test_uri_73"
    (uri-parse "httppath#fragment")
    nil
)
(test "test_uri_74"
    (uri-parse "httppath")
    nil
)
(test "test_uri_75"
    (uri-parse "http?query#fragment")
    nil
)
(test "test_uri_76"
    (uri-parse "http#fragment")
    nil
)
(test "test_uri_77"
    (uri-parse "http?query")
    nil
)
(test "test_uri_78"
    (uri-parse "http")
    nil
)
(test "test_uri_79"
    (uri-parse ":/path/subpath?query#fragment")
    nil
)
(test "test_uri_80"
    (uri-parse ":/path/subpath?query")
    nil
)
(test "test_uri_81"
    (uri-parse ":/path/subpath#fragment")
    nil
)
(test "test_uri_82"
    (uri-parse ":/path/subpath")
    nil
)
(test "test_uri_83"
    (uri-parse ":/path?query#fragment")
    nil
)
(test "test_uri_84"
    (uri-parse ":/path?query")
    nil
)
(test "test_uri_85"
    (uri-parse ":/path#fragment")
    nil
)
(test "test_uri_86"
    (uri-parse ":/path")
    nil
)
(test "test_uri_87"
    (uri-parse ":/?query#fragment")
    nil
)
(test "test_uri_88"
    (uri-parse ":/#fragment")
    nil
)
(test "test_uri_89"
    (uri-parse ":/?query")
    nil
)
(test "test_uri_90"
    (uri-parse ":/")
    nil
)
(test "test_uri_91"
    (uri-parse ":path/subpath?query#fragment")
    nil
)
(test "test_uri_92"
    (uri-parse ":path/subpath?query")
    nil
)
(test "test_uri_93"
    (uri-parse ":path/subpath#fragment")
    nil
)
(test "test_uri_94"
    (uri-parse ":path/subpath")
    nil
)
(test "test_uri_95"
    (uri-parse ":path?query#fragment")
    nil
)
(test "test_uri_96"
    (uri-parse ":path?query")
    nil
)
(test "test_uri_97"
    (uri-parse ":path#fragment")
    nil
)
(test "test_uri_98"
    (uri-parse ":path")
    nil
)
(test "test_uri_99"
    (uri-parse ":?query#fragment")
    nil
)
(test "test_uri_100"
    (uri-parse ":#fragment")
    nil
)
(test "test_uri_101"
    (uri-parse ":?query")
    nil
)
(test "test_uri_102"
    (uri-parse ":")
    nil
)





(format t "~%Run ~D tests: passed ~D, failed ~D" tot passed failed)