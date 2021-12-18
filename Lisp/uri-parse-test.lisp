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
        :port "80"
    )
)
(test "test-scheme2"
    (uri-parse "h11ps://google.com")
    (prepare-uri 
        :scheme "h11ps"
        :host "google.com"
        :port "80"
    )
)
(test "test-scheme3"
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
(test "test-userinfo1"
    (uri-parse "http://userinfo@host")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-userinfo2"
    (uri-parse "http://user_info@host")
    (prepare-uri
        :scheme "http"
        :userinfo "user_info"
        :host "host"
        :port "80"
    )
)
(test "test-userinfo3"
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
(test "test-host1"
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-host2"
    (uri-parse "scheme://userinfo@host")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-host3"
    (uri-parse "scheme://host:123")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "123"
    )
)
(test "test-host4"
    (uri-parse "scheme://userinfo@host:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host"
        :port "123"
    )
)
(test "test-host5"
    (uri-parse "scheme://host/path")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "path"
    )
)
(test "test-host6"
    (uri-parse "scheme://userinfo@host.com:123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "host.com"
        :port "123"
    )
)
(test "test-host7"
    (uri-parse "scheme://123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port "80"
    )
)
(test "test-host8"
    (uri-parse "scheme://123.123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123.123"
        :port "80"
    )
)
(test "test-host9"
    (uri-parse "scheme://123.123.123.1233")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.1233"
        :port "80"
    )
)
(test "test-host10"
    (uri-parse "scheme://257.257.257.257")
    (prepare-uri
        :scheme "scheme"
        :host "257.257.257.257"
        :port "80"
    )
)
(test "test-host11"
    (uri-parse "scheme://123.123.123")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123"
        :port "80"
    )
)
(test "test-host12"
    (uri-parse "scheme://userinfo@123.123.123.123")
    (prepare-uri
        :scheme "scheme"
        :userinfo "userinfo"
        :host "123.123.123.123"
        :port "80"
    )
)
(test "test-host13"
    (uri-parse "scheme://123.123.123.123:321")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port "321"
    )
)
(test "test-host14"
    (uri-parse "scheme://123.123.123.123/path")
    (prepare-uri
        :scheme "scheme"
        :host "123.123.123.123"
        :port "80"
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
    (uri-parse "scheme://host")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
    )
)
(test "test-path5"
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
    (uri-parse "scheme://host/pro va")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "pro%20va"
    )
)
(test "test-path7"
    (uri-parse "scheme://host/pro.va")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :path "pro.va"
    )
)
(test "test-path6"
    (uri-parse "scheme://host/path#fragment")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
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
(test "test-query1"
    (uri-parse "scheme://host/?qu ery")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
        :query "qu%20ery"
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
(test "test-fragment5"
    (uri-parse "scheme://host/#fr ag")
    (prepare-uri
        :scheme "scheme"
        :host "host"
        :port "80"
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
(test "test-tel_2"
    (uri-parse "tel:user info")
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
        :port "80"
        :path "id44(id8)"
    )
)
(test "test-zos2"
    (uri-parse "zos://userinfo@host/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "id44(id8)"
    )
)
(test "test-zos3"
    (uri-parse "zos://userinfo@host:4832/id44(id8)")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port "4832"
        :path "id44(id8)"
    )
)
(test "test-zos4"
    (uri-parse "zos://userinfo@host:4832/id44(id8)?query")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port "4832"
        :path "id44(id8)"
        :query "query"
    )
)
(test "test-zos5"
    (uri-parse "zos://userinfo@host:4832/id44(id8)?query#fragment")
    (prepare-uri
        :scheme "zos"
        :userinfo "userinfo"
        :host "host"
        :port "4832"
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
        :port "80"
        :path "id.44(id8)"
    )
)
(test "test-zos7"
    (uri-parse "zos://host/i.d.4.4(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port "80"
        :path "i.d.4.4(id8)"
    )
)
(test "test-zos8"
    (uri-parse "zos://host/i.d.4.4")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port "80"
        :path "i.d.4.4"
    )
)
(test "test-zos9"
    (uri-parse "zos://host/id..prova")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port "80"
        :path "id..prova"
    )
)
(test "test-zos10"
    (uri-parse "zos://host/id..prova(id8)")
    (prepare-uri
        :scheme "zos"
        :host "host"
        :port "80"
        :path "id..prova(id8)"
    )
)
(test "test-zos_1"
    (uri-parse "zos://host")
    nil
)
(test "test-zos_2"
    (uri-parse "zos://host/.i.d")
    nil
)
(test "test-zos_3"
    (uri-parse "zos://host/.")
    nil
)
(test "test-zos_4"
    (uri-parse "zos://host/")
    nil
)
(test "test-zos_5"
    (uri-parse "zos://host/a012345678901234567890123456789012345678901234(id)")
    nil
)
(test "test-zos_6"
    (uri-parse "zos://host/path(a012345678)")
    nil
)
(test "test-zos_7"
    (uri-parse "zos://host/a012345678901234567890123456789012345678901234(a012345678)")
    nil
)
(test "test-zos_8"
    (uri-parse "zos://host/(id44)")
    nil
)
(test "test-zos_9"
    (uri-parse "zos://host/pi@ppo(id44)")
    nil
)
(test "test-zos_10"
    (uri-parse "zos://host/pip po(id44)")
    nil
)
(test "test-zos_11"
    (uri-parse "zos://host/pippo.mar co(id44)")
    nil
)
(test "test-zos_12"
    (uri-parse "zos://host/pippo(id.44)")
    nil
)
(test "test-zos_13"
    (uri-parse "zos://host/pippo(id .44)")
    nil
)
(test "test-zos_14"
    (uri-parse "zos://host/pippo()")
    nil
)
(test "test-zos_15"
    (uri-parse "zos://host/pippo( )")
    nil
)
(test "test-zos_16"
    (uri-parse "zos://host/pippo(")
    nil
)
(test "test-zos_17"
    (uri-parse "zos://host/pippo)")
    nil
)
(test "test-zos_18"
    (uri-parse "zos://host/id.")
    nil
)
(test "test-zos_19"
    (uri-parse "zos://host/id(1)")
    nil
)
(test "test-zos_20"
    (uri-parse "zos://host/id(1id8)")
    nil
)
(test "test-zos_21"
    (uri-parse "zos://host/id..prova..")
    nil
)
(test "test-zos_22"
    (uri-parse "zos://host/..id..prova")
    nil
)
(test "test-zos_23"
    (uri-parse "zos://host/id..prova..(id8)")
    nil
)
(test "test-zos_24"
    (uri-parse "zos://host/..id..prova(id8)")
    nil
)
(test "test-zos_25"
    (uri-parse "zos://host/.i.d(id8)")
    nil
)
(test "test-zos_26"
    (uri-parse "zos://host/.(id8)")
    nil
)
(test "test-zos_27"
    (uri-parse "zos://host/id.(id8)")
    nil
)

; TEST INTEGRATION ---- PRIMO TIPO DI URI
(test "test-primo-tipo1"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo2"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo3"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo4"
    (uri-parse "http://userinfo@host.subhost:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
    )
)
(test "test-primo-tipo5"
    (uri-parse "http://userinfo@host.subhost:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo6"
    (uri-parse "http://userinfo@host.subhost:123/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo7"
    (uri-parse "http://userinfo@host.subhost:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo8"
    (uri-parse "http://userinfo@host.subhost:123/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :path "path"
    )
)
(test "test-primo-tipo9"
    (uri-parse "http://userinfo@host.subhost:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo10"
    (uri-parse "http://userinfo@host.subhost:123/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :query "query"
    )
)

(test "test-primo-tipo11"
    (uri-parse "http://userinfo@host.subhost:123/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
        :fragment "fragment"
    )
)
(test "test-primo-tipo12"
    (uri-parse "http://userinfo@host.subhost:123/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
    )
)
(test "test-primo-tipo13"
    (uri-parse "http://userinfo@host.subhost:123")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "123"
    )
)
(test "test-primo-tipo14"
    (uri-parse "http://userinfo@host.subhost/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo15"
    (uri-parse "http://userinfo@host.subhost/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo16"
    (uri-parse "http://userinfo@host.subhost/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo17"
    (uri-parse "http://userinfo@host.subhost/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
    )
)
(test "test-primo-tipo18"
    (uri-parse "http://userinfo@host.subhost/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo19"
    (uri-parse "http://userinfo@host.subhost/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo20"
    (uri-parse "http://userinfo@host.subhost/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo21"
    (uri-parse "http://userinfo@host.subhost/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :path "path"
    )
)
(test "test-primo-tipo22"
    (uri-parse "http://userinfo@host.subhost/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo23"
    (uri-parse "http://userinfo@host.subhost/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :query "query"
    )
)

(test "test-primo-tipo24"
    (uri-parse "http://userinfo@host.subhost/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
        :fragment "fragment"
    )
)
(test "test-primo-tipo25"
    (uri-parse "http://userinfo@host.subhost/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
    )
)
(test "test-primo-tipo26"
    (uri-parse "http://userinfo@host.subhost")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host.subhost"
        :port "80"
    )
)
(test "test-primo-tipo27"
    (uri-parse "http://host.subhost:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo28"
    (uri-parse "http://host.subhost:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo29"
    (uri-parse "http://host.subhost:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo30"
    (uri-parse "http://host.subhost:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path/subpath"
    )
)
(test "test-primo-tipo31"
    (uri-parse "http://host.subhost:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo32"
    (uri-parse "http://host.subhost:123/path?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo33"
    (uri-parse "http://host.subhost:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo34"
    (uri-parse "http://host.subhost:123/path")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :path "path"
    )
)
(test "test-primo-tipo35"
    (uri-parse "http://host.subhost:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo36"
    (uri-parse "http://host.subhost:123/?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :query "query"
    )
)

(test "test-primo-tipo37"
    (uri-parse "http://host.subhost:123/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
        :fragment "fragment"
    )
)
(test "test-primo-tipo38"
    (uri-parse "http://host.subhost:123/")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
    )
)
(test "test-primo-tipo39"
    (uri-parse "http://host.subhost:123")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "123"
    )
)
(test "test-primo-tipo40"
    (uri-parse "http://host.subhost/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo41"
    (uri-parse "http://host.subhost/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo42"
    (uri-parse "http://host.subhost/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo43"
    (uri-parse "http://host.subhost/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path/subpath"
    )
)
(test "test-primo-tipo44"
    (uri-parse "http://host.subhost/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo45"
    (uri-parse "http://host.subhost/path?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo46"
    (uri-parse "http://host.subhost/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo47"
    (uri-parse "http://host.subhost/path")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :path "path"
    )
)
(test "test-primo-tipo48"
    (uri-parse "http://host.subhost/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo49"
    (uri-parse "http://host.subhost/?query")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :query "query"
    )
)

(test "test-primo-tipo50"
    (uri-parse "http://host.subhost/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
        :fragment "fragment"
    )
)
(test "test-primo-tipo51"
    (uri-parse "http://host.subhost/")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
    )
)
(test "test-primo-tipo52"
    (uri-parse "http://host.subhost")
    (prepare-uri
        :scheme "http"
        :host "host.subhost"
        :port "80"
    )
)




(test "test-primo-tipo53"
    (uri-parse "http://userinfo@host:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo54"
    (uri-parse "http://userinfo@host:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo55"
    (uri-parse "http://userinfo@host:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo56"
    (uri-parse "http://userinfo@host:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path/subpath"
    )
)
(test "test-primo-tipo57"
    (uri-parse "http://userinfo@host:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo58"
    (uri-parse "http://userinfo@host:123/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo59"
    (uri-parse "http://userinfo@host:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo60"
    (uri-parse "http://userinfo@host:123/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :path "path"
    )
)
(test "test-primo-tipo61"
    (uri-parse "http://userinfo@host:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo62"
    (uri-parse "http://userinfo@host:123/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :query "query"
    )
)

(test "test-primo-tipo63"
    (uri-parse "http://userinfo@host:123/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
        :fragment "fragment"
    )
)
(test "test-primo-tipo64"
    (uri-parse "http://userinfo@host:123/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
    )
)
(test "test-primo-tipo65"
    (uri-parse "http://userinfo@host:123")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "123"
    )
)
(test "test-primo-tipo66"
    (uri-parse "http://userinfo@host/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo67"
    (uri-parse "http://userinfo@host/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo68"
    (uri-parse "http://userinfo@host/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo69"
    (uri-parse "http://userinfo@host/path/subpath")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path/subpath"
    )
)
(test "test-primo-tipo70"
    (uri-parse "http://userinfo@host/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo71"
    (uri-parse "http://userinfo@host/path?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo72"
    (uri-parse "http://userinfo@host/path#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo73"
    (uri-parse "http://userinfo@host/path")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :path "path"
    )
)
(test "test-primo-tipo74"
    (uri-parse "http://userinfo@host/?query#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo75"
    (uri-parse "http://userinfo@host/?query")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :query "query"
    )
)

(test "test-primo-tipo76"
    (uri-parse "http://userinfo@host/#fragment")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
        :fragment "fragment"
    )
)
(test "test-primo-tipo77"
    (uri-parse "http://userinfo@host/")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-primo-tipo78"
    (uri-parse "http://userinfo@host")
    (prepare-uri
        :scheme "http"
        :userinfo "userinfo"
        :host "host"
        :port "80"
    )
)
(test "test-primo-tipo79"
    (uri-parse "http://host:123/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo80"
    (uri-parse "http://host:123/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo81"
    (uri-parse "http://host:123/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo82"
    (uri-parse "http://host:123/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path/subpath"
    )
)
(test "test-primo-tipo83"
    (uri-parse "http://host:123/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo84"
    (uri-parse "http://host:123/path?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo85"
    (uri-parse "http://host:123/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo86"
    (uri-parse "http://host:123/path")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :path "path"
    )
)
(test "test-primo-tipo87"
    (uri-parse "http://host:123/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo88"
    (uri-parse "http://host:123/?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :query "query"
    )
)

(test "test-primo-tipo89"
    (uri-parse "http://host:123/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
        :fragment "fragment"
    )
)
(test "test-primo-tipo90"
    (uri-parse "http://host:123/")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
    )
)
(test "test-primo-tipo91"
    (uri-parse "http://host:123")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "123"
    )
)
(test "test-primo-tipo92"
    (uri-parse "http://host/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo93"
    (uri-parse "http://host/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-primo-tipo94"
    (uri-parse "http://host/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-primo-tipo95"
    (uri-parse "http://host/path/subpath")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path/subpath"
    )
)
(test "test-primo-tipo96"
    (uri-parse "http://host/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo97"
    (uri-parse "http://host/path?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path"
        :query "query"
    )
)
(test "test-primo-tipo98"
    (uri-parse "http://host/path#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-primo-tipo99"
    (uri-parse "http://host/path")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :path "path"
    )
)
(test "test-primo-tipo100"
    (uri-parse "http://host/?query#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-primo-tipo101"
    (uri-parse "http://host/?query")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :query "query"
    )
)

(test "test-primo-tipo102"
    (uri-parse "http://host/#fragment")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
        :fragment "fragment"
    )
)
(test "test-primo-tipo103"
    (uri-parse "http://host/")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
    )
)
(test "test-primo-tipo104"
    (uri-parse "http://host")
    (prepare-uri
        :scheme "http"
        :host "host"
        :port "80"
    )
)
(test "test-primo-tipo_1"
    (uri-parse "http://")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path/subpath?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path/subpath?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path/subpath#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path/subpath")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///path")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http:///")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http://")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http://host?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http://host?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http://host#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path/subpath?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path/subpath?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path/subpath#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path/subpath")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/path")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host/")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "://host")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path/subpath?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path/subpath?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path/subpath#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path/subpath")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/path")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/?query#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/?query")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/#fragment")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host/")
    nil
)
(test "test-primo-tipo_1"
    (uri-parse "http//host")
    nil
)

; INTEGRATION TEST ---- secondo tipo di uri
(test "test-secondo-tipo1"
    (uri-parse "http:/path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo2"
    (uri-parse "http:/path/subpath?query")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-secondo-tipo3"
    (uri-parse "http:/path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo4"
    (uri-parse "http:/path/subpath")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
    )
)
(test "test-secondo-tipo5"
    (uri-parse "http:/path?query#fragment")
    (prepare-uri
        :scheme "http"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo6"
    (uri-parse "http:/path?query")
    (prepare-uri
        :scheme "http"
        :path "path"
        :query "query"
    )
)
(test "test-secondo-tipo7"
    (uri-parse "http:/path#fragment")
    (prepare-uri
        :scheme "http"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo8"
    (uri-parse "http:/path")
    (prepare-uri
        :scheme "http"
        :path "path"
    )
)
(test "test-secondo-tipo9"
    (uri-parse "http:/?query#fragment")
    (prepare-uri
        :scheme "http"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo10"
    (uri-parse "http:/#fragment")
    (prepare-uri
        :scheme "http"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo11"
    (uri-parse "http:/?query")
    (prepare-uri
        :scheme "http"
        :query "query"
    )
)
(test "test-secondo-tipo12"
    (uri-parse "http:/")
    (prepare-uri
        :scheme "http"
    )
)
(test "test-secondo-tipo13"
    (uri-parse "http:path/subpath?query#fragment")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo14"
    (uri-parse "http:path/subpath?query")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :query "query"
    )
)
(test "test-secondo-tipo15"
    (uri-parse "http:path/subpath#fragment")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo16"
    (uri-parse "http:path/subpath")
    (prepare-uri
        :scheme "http"
        :path "path/subpath"
    )
)
(test "test-secondo-tipo17"
    (uri-parse "http:path?query#fragment")
    (prepare-uri
        :scheme "http"
        :path "path"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo18"
    (uri-parse "http:path?query")
    (prepare-uri
        :scheme "http"
        :path "path"
        :query "query"
    )
)
(test "test-secondo-tipo19"
    (uri-parse "http:path#fragment")
    (prepare-uri
        :scheme "http"
        :path "path"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo20"
    (uri-parse "http:path")
    (prepare-uri
        :scheme "http"
        :path "path"
    )
)
(test "test-secondo-tipo21"
    (uri-parse "http:?query#fragment")
    (prepare-uri
        :scheme "http"
        :query "query"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo22"
    (uri-parse "http:#fragment")
    (prepare-uri
        :scheme "http"
        :fragment "fragment"
    )
)
(test "test-secondo-tipo23"
    (uri-parse "http:?query")
    (prepare-uri
        :scheme "http"
        :query "query"
    )
)
(test "test-secondo-tipo24"
    (uri-parse "http:")
    (prepare-uri
        :scheme "http"
    )
)

(test "test-secondo-tipo_1"
    (uri-parse "http/path/subpath?query#fragment")
    nil
)
(test "test-secondo-tipo_2"
    (uri-parse "http/path/subpath?query")
    nil
)
(test "test-secondo-tipo_3"
    (uri-parse "http/path/subpath#fragment")
    nil
)
(test "test-secondo-tipo_4"
    (uri-parse "http/path/subpath")
    nil
)
(test "test-secondo-tipo_5"
    (uri-parse "http/path?query#fragment")
    nil
)
(test "test-secondo-tipo_6"
    (uri-parse "http/path?query")
    nil
)
(test "test-secondo-tipo_7"
    (uri-parse "http/path#fragment")
    nil
)
(test "test-secondo-tipo_8"
    (uri-parse "http/path")
    nil
)
(test "test-secondo-tipo_9"
    (uri-parse "http/?query#fragment")
    nil
)
(test "test-secondo-tipo_10"
    (uri-parse "http/#fragment")
    nil
)
(test "test-secondo-tipo_11"
    (uri-parse "http/?query")
    nil
)
(test "test-secondo-tipo_12"
    (uri-parse "http/")
    nil
)
(test "test-secondo-tipo_13"
    (uri-parse "httppath/subpath?query#fragment")
    nil
)
(test "test-secondo-tipo_14"
    (uri-parse "httppath/subpath?query")
    nil
)
(test "test-secondo-tipo_15"
    (uri-parse "httppath/subpath#fragment")
    nil
)
(test "test-secondo-tipo_16"
    (uri-parse "httppath/subpath")
    nil
)
(test "test-secondo-tipo_17"
    (uri-parse "httppath?query#fragment")
    nil
)
(test "test-secondo-tipo_18"
    (uri-parse "httppath?query")
    nil
)
(test "test-secondo-tipo_19"
    (uri-parse "httppath#fragment")
    nil
)
(test "test-secondo-tipo_20"
    (uri-parse "httppath")
    nil
)
(test "test-secondo-tipo_21"
    (uri-parse "http?query#fragment")
    nil
)
(test "test-secondo-tipo_22"
    (uri-parse "http#fragment")
    nil
)
(test "test-secondo-tipo_23"
    (uri-parse "http?query")
    nil
)
(test "test-secondo-tipo_24"
    (uri-parse "http")
    nil
)
(test "test-secondo-tipo_25"
    (uri-parse ":/path/subpath?query#fragment")
    nil
)
(test "test-secondo-tipo_26"
    (uri-parse ":/path/subpath?query")
    nil
)
(test "test-secondo-tipo_27"
    (uri-parse ":/path/subpath#fragment")
    nil
)
(test "test-secondo-tipo_28"
    (uri-parse ":/path/subpath")
    nil
)
(test "test-secondo-tipo_29"
    (uri-parse ":/path?query#fragment")
    nil
)
(test "test-secondo-tipo_30"
    (uri-parse ":/path?query")
    nil
)
(test "test-secondo-tipo_31"
    (uri-parse ":/path#fragment")
    nil
)
(test "test-secondo-tipo_32"
    (uri-parse ":/path")
    nil
)
(test "test-secondo-tipo_33"
    (uri-parse ":/?query#fragment")
    nil
)
(test "test-secondo-tipo_34"
    (uri-parse ":/#fragment")
    nil
)
(test "test-secondo-tipo_35"
    (uri-parse ":/?query")
    nil
)
(test "test-secondo-tipo_36"
    (uri-parse ":/")
    nil
)
(test "test-secondo-tipo_37"
    (uri-parse ":path/subpath?query#fragment")
    nil
)
(test "test-secondo-tipo_38"
    (uri-parse ":path/subpath?query")
    nil
)
(test "test-secondo-tipo_39"
    (uri-parse ":path/subpath#fragment")
    nil
)
(test "test-secondo-tipo_40"
    (uri-parse ":path/subpath")
    nil
)
(test "test-secondo-tipo_41"
    (uri-parse ":path?query#fragment")
    nil
)
(test "test-secondo-tipo_42"
    (uri-parse ":path?query")
    nil
)
(test "test-secondo-tipo_43"
    (uri-parse ":path#fragment")
    nil
)
(test "test-secondo-tipo_44"
    (uri-parse ":path")
    nil
)
(test "test-secondo-tipo_45"
    (uri-parse ":?query#fragment")
    nil
)
(test "test-secondo-tipo_46"
    (uri-parse ":#fragment")
    nil
)
(test "test-secondo-tipo_47"
    (uri-parse ":?query")
    nil
)
(test "test-secondo-tipo_48"
    (uri-parse ":")
    nil
)





(format t "~%Run ~D tests: passed ~D, failed ~D" tot passed failed)