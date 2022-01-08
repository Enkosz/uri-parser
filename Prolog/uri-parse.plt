:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', [], 'google.com', 80, [], [], [])).
test(schema2) :- uri_parse("h11ps://google.com", uri('h11ps', [], 'google.com', 80, [], [], [])).
test(schema3) :- uri_parse("_http_://google.com", uri('_http_', [], 'google.com', 80, [], [], [])).
test(schema4) :- uri_parse("s:", uri(s, [], [], 80, [], [], [])).
test(schema5) :- uri_parse("ss:", uri(ss, [], [], 80, [], [], [])).    
test(schema6) :- uri_parse("s1:", uri('s1', [], [], 80, [], [], [])).  
test(schema7) :- uri_parse("s+:", uri('s+', [], [], 80, [], [], [])).  
test(schema8) :- uri_parse("s-:", uri('s-', [], [], 80, [], [], [])).  
test(schema9) :- uri_parse("s.:", uri('s.', [], [], 80, [], [], [])).  
test(schema10) :- uri_parse("+:", uri('+', [], [], 80, [], [], [])).  
test(schema11) :- uri_parse("1:", uri('1', [], [], 80, [], [], [])).                  
test(schema_1) :- \+(uri_parse("://google.com", _)).
test(schema_2) :- \+(uri_parse("@://google.com", _)).
test(schema_3) :- \+(uri_parse("h:h://google.com", _)).
test(schema_4) :- \+(uri_parse("/hh://google.com", _)).
test(schema_5) :- \+(uri_parse("h#h://google.com", _)).
test(schema_6) :- \+(uri_parse("h?h://google.com", _)).
test(schema_7) :- \+(uri_parse(":://google.com", _)).
test(schema_8) :- \+(uri_parse("s/:", _)).
test(schema_9) :- \+(uri_parse("s?:", _)). 
test(schema_10) :- \+(uri_parse("s#:", _)). 
test(schema_11) :- \+(uri_parse("s@:", _)). 
test(schema_12) :- \+(uri_parse("s::", _)). 
test(schema_13) :- \+(uri_parse(":", _)).                                   
test(schema_14) :- \+(uri_parse("ù:", _)).                                          


% TEST USERINFO
test(userinfo1) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', 80, [], [], [])).
test(userinfo2) :- uri_parse("http://user_info@host", uri('http', 'user_info', 'host', 80, [], [], [])).
test(userinfo3) :- uri_parse("http://user123info@host", uri('http', 'user123info', 'host', 80, [], [], [])).
test(userinfo4) :- uri_parse("http://user.@host", uri('http', 'user.', 'host', 80, [], [], [])).
test(userinfo5) :- uri_parse("s://u%20i@host", uri(s, 'u%20i', host, 80, [], [], [])).
test(userinfo6) :- uri_parse("s://u-@host", uri(s, 'u-', host, 80, [], [], [])).
test(userinfo7) :- uri_parse("s://u_@host", uri(s, 'u_', host, 80, [], [], [])).
test(userinfo8) :- uri_parse("http://userin fo@host", uri(http, 'userin fo', host, 80, [], [], [])).
test(userinfo_1) :- \+(uri_parse("http://user@info@host", _)).
test(userinfo_2) :- \+(uri_parse("http://userin:fo@host", _)).
test(userinfo_3) :- \+(uri_parse("http://userin/fo@host", _)).
test(userinfo_4) :- \+(uri_parse("http://userin?fo@host", _)).
test(userinfo_5) :- \+(uri_parse("http://userin#fo@host", _)).
test(userinfo_6) :- \+(uri_parse("s://@host", _)).
test(userinfo_7) :- \+(uri_parse("http://userinfo@", _)).
test(userinfo_8) :- \+(uri_parse("http://@", _)).


% TEST HOST
test(host1) :- uri_parse("scheme://host", uri('scheme', [], 'host', 80, [], [], [])).
test(host2) :- uri_parse("scheme://userinfo@host", uri('scheme', 'userinfo', 'host', 80, [], [], [])).
test(host3) :- uri_parse("scheme://host:123", uri('scheme', [], 'host', 123, [], [], [])).
test(host4) :- uri_parse("scheme://userinfo@host:123", uri('scheme', 'userinfo', 'host', 123, [], [], [])).
test(host5) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', 80, 'path', [], [])).
test(host6) :- uri_parse("scheme://userinfo@host.com:123", uri('scheme', 'userinfo', 'host.com', 123, [], [], [])).
test(host7) :- uri_parse("scheme://123.123.123.123", uri('scheme', [], '123.123.123.123', 80, [], [], [])).
test(host8) :- uri_parse("scheme://123.123.123.123.123", uri('scheme', [], '123.123.123.123.123', 80, [], [], [])).
test(host9) :- uri_parse("scheme://123.123.123.1233", uri('scheme', [], '123.123.123.1233', 80, [], [], [])).
test(host10) :- uri_parse("scheme://257.257.257.257", uri('scheme', [], '257.257.257.257', 80, [], [], [])).
test(host11) :- uri_parse("scheme://123.123.123", uri('scheme', [], '123.123.123', 80, [], [], [])).
test(host12) :- uri_parse("scheme://userinfo@123.123.123.123", uri('scheme', 'userinfo', '123.123.123.123', 80, [], [], [])).
test(host13) :- uri_parse("scheme://123.123.123.123:123", uri('scheme', [], '123.123.123.123', 123, [], [], [])).
test(host14) :- uri_parse("scheme://123.123.123.123/path", uri('scheme', [], '123.123.123.123', 80, 'path', [], [])).
test(host15) :- uri_parse("scheme://userinfo@ho st:123", uri('scheme', 'userinfo', 'ho st', 123, [], [], [])).
test(host16) :- uri_parse("scheme://host ", uri('scheme', [], 'host ', 80, [], [], [])).
test(host17) :- uri_parse("scheme:// host", uri('scheme', [], ' host', 80, [], [], [])).
test(host_1) :- \+(uri_parse("scheme://userinfo@ho?st:123", _)).
test(host_2) :- \+(uri_parse("scheme://userinfo@ho@st:123", _)).
test(host_3) :- \+(uri_parse("scheme://userinfo@ho:st:123", _)).
test(host_4) :- \+(uri_parse("scheme://userinfo@ho/st:123", _)).
test(host_5) :- \+(uri_parse("scheme://userinfo@ho#st:123", _)).
test(host_6) :- \+(uri_parse("s://", _)).
test(host_7) :- \+(uri_parse("scheme://host..com", _)).
test(host_8) :- \+(uri_parse("scheme://host.", _)).
test(host_9) :- \+(uri_parse("scheme://host:", _)).
test(host_10) :- \+(uri_parse("scheme://host@", _)).
test(host_11) :- \+(uri_parse("scheme://host?", _)).
test(host_12) :- \+(uri_parse("scheme://host#", _)).
test(host_13) :- \+(uri_parse("scheme://#host", _)).
test(host_14) :- \+(uri_parse("scheme://.host", _)).
test(host_15) :- \+(uri_parse("scheme:///host", _)).
test(host_16) :- \+(uri_parse("scheme://@host", _)).
test(host_17) :- \+(uri_parse("scheme://:host", _)).
test(host_18) :- \+(uri_parse("scheme://?host", _)).




% TEST PORT
test(port1) :- uri_parse("scheme://host:123", uri('scheme', [], 'host', 123, [], [], [])).
test(port2) :- uri_parse("scheme://host:123/path", uri('scheme', [], 'host', 123, 'path', [], [])).
test(port3) :- uri_parse("http://host:123", uri('http', [], 'host', 123, [], [], [])).
test(port4) :- uri_parse("http://host", uri('http', [], 'host', 80, [], [], [])).
test(port5) :- uri_parse("https://host:123", uri('https', [], 'host', 123, [], [], [])).
test(port6) :- uri_parse("https://host", uri('https', [], 'host', 80, [], [], [])).
test(port_1) :- \+(uri_parse("scheme://host:1_23", _)).
test(port_2) :- \+(uri_parse("scheme://host:1a23", _)).
test(port_3) :- \+(uri_parse("scheme://host:", _)).
test(port_4) :- \+(uri_parse("scheme://host: ", _)).
test(port_5) :- \+(uri_parse("s://:110", _)).
test(port_6) :- \+(uri_parse("s://host:a",_)).
test(port_7) :- \+(uri_parse("s://host:12 2",_)).

% TEST PATH
test(path1) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', 80, 'path', [], [])).
test(path2) :- uri_parse("scheme://host/path/prova", uri('scheme', [], 'host', 80, 'path/prova', [], [])).
test(path3) :- uri_parse("scheme://host/", uri('scheme', [], 'host', 80, [], [], [])).
test(path4) :- uri_parse("scheme://host", uri('scheme', [], 'host', 80, [], [], [])).
test(path5) :- uri_parse("scheme://host/path?query", uri('scheme', [], 'host', 80, 'path', 'query', [])).
test(path6) :- uri_parse("scheme://host/pro va", uri('scheme', [], 'host', 80, 'pro va', [], [])).
test(path7) :- uri_parse("scheme://host/pro.va", uri('scheme', [], 'host', 80, 'pro.va', [], [])).
test(path8) :- uri_parse("scheme://host/path#fragment", uri('scheme', [], 'host', 80, 'path', [], 'fragment')).
test(path_1) :- \+(uri_parse("scheme://host/path/", _)).
test(path_2) :- \+(uri_parse("scheme://host/p:ath", _)).
test(path_3) :- \+(uri_parse("scheme://host/p@ath", _)).
test(path_4) :- \+(uri_parse("scheme://host//", _)).
test(path_5) :- \+(uri_parse("scheme://host/@path", _)).
test(path_6) :- \+(uri_parse("scheme://host/:path", _)).
test(path_7) :- \+(uri_parse("scheme://host/path@", _)).
test(path_8) :- \+(uri_parse("scheme://host/path:", _)).
test(path_9) :- \+(uri_parse("scheme://host/path//com", _)).
test(path_10) :- \+(uri_parse("scheme://host//path", _)).

% TEST QUERY
test(query1) :- uri_parse("scheme://host/?query", uri('scheme', [], 'host', 80, [], 'query', [])).
test(query2) :- uri_parse("scheme://host/?qu ery", uri('scheme', [], 'host', 80, [], 'qu ery', [])).
test(query3) :- uri_parse("scheme://host/?qu.ery", uri('scheme', [], 'host', 80, [], 'qu.ery', [])).
test(query4) :- uri_parse("scheme://host/?qu:ery", uri('scheme', [], 'host', 80, [], 'qu:ery', [])).
test(query5) :- uri_parse("scheme://host/?qu@ery", uri('scheme', [], 'host', 80, [], 'qu@ery', [])).
test(query6) :- uri_parse("scheme://host/?qu/ery", uri('scheme', [], 'host', 80, [], 'qu/ery', [])).
test(query7) :- uri_parse("scheme://host/?qu?ery", uri('scheme', [], 'host', 80, [], 'qu?ery', [])).
test(query8) :- uri_parse("scheme://host", uri('scheme', [], 'host', 80, [], [], [])).
test(query9) :- uri_parse("scheme://host/", uri('scheme', [], 'host', 80, [], [], [])).
test(query10) :- uri_parse("scheme://host/path?query#fragment", uri('scheme', [], 'host', 80, 'path', 'query', 'fragment')).
test(query_1) :- \+(uri_parse("scheme://host/?", _)).
test(query_2) :- \+(uri_parse("scheme://host/?#", _)).
test(query_3) :- \+(uri_parse("scheme://host?query", _)).

% TEST FRAGMENT
test(fragment1) :- uri_parse("scheme://host/#frag", uri('scheme', [], 'host', 80, [], [], 'frag')).
test(fragment2) :- uri_parse("scheme://host/path?query#frag", uri('scheme', [], 'host', 80, 'path', 'query', 'frag')).
test(fragment3) :- uri_parse("scheme://host/?query#frag", uri('scheme', [], 'host', 80, [], 'query', 'frag')).
test(fragment4) :- uri_parse("scheme://host", uri('scheme', [], 'host', 80, [], [], [])).
test(fragment5) :- uri_parse("scheme://host/#fr ag", uri('scheme', [], 'host', 80, [], [], 'fr ag')).
test(fragment_1) :- \+(uri_parse("scheme://host/#", _)).
test(fragment_2) :- \+(uri_parse("scheme://host#", _)).

% TEST SCHEMA MAILTO
test(mailto1) :- uri_parse("mailto:userinfo", uri('mailto', 'userinfo', [], 80, [], [], [])).
test(mailto2) :- uri_parse("mailto:userinfo@host", uri('mailto', 'userinfo', 'host', 80, [], [], [])).
test(mailto3) :- uri_parse("mailto:", uri('mailto', [], [], 80, [], [], [])).
test(mailto4) :- uri_parse("mailto:userinfo@host.com", uri('mailto', 'userinfo', 'host.com', 80, [], [], [])).
test(mailto5) :- uri_parse("mailto:user.info",uri('mailto', 'user.info', [], 80, [], [], [])).
test(mailto_1) :- \+(uri_parse("mailto:userinfo@", _)).
test(mailto_2) :- \+(uri_parse("mailto:userinfo@host?query", _)).
test(mailto_3) :- \+(uri_parse("mailto:userinfo@host/path", _)).

% TEST SCHEMA FAX
test(fax1) :- uri_parse("fax:userinfo", uri('fax', 'userinfo', [], 80, [], [], [])).
test(fax2) :- uri_parse("fax:user123info", uri('fax', 'user123info', [], 80, [], [], [])).
test(fax3) :- uri_parse("fax:", uri('fax', [], [], 80, [], [], [])).
test(fax4) :- uri_parse("fax:user info", uri('fax', 'user info', [], 80, [], [], [])).
test(fax_1) :- \+(uri_parse("fax:userinfo@host", _)).
test(fax_2) :- \+(uri_parse("fax:userinfo/path", _)).

% TEST SCHEMA TEL
test(tel1) :- uri_parse("tel:userinfo", uri('tel', 'userinfo', [], 80, [], [], [])).
test(tel2) :- uri_parse("tel:user123info", uri('tel', 'user123info', [], 80, [], [], [])).
test(tel3) :- uri_parse("tel:0293564242", uri('tel', '0293564242', [], 80, [], [], [])).
test(tel4) :- uri_parse("tel:", uri('tel', [], [], 80, [], [], [])).
test(tel5) :- uri_parse("tel:user info",uri('tel', 'user info', [], 80, [], [], [])).
test(tel_1) :- \+(uri_parse("tel:userinfo@host", _)).
test(tel_2) :- \+(uri_parse("tel:userinfo/path", _)).

% TEST SCHEMA NEWS
test(news1) :- uri_parse("news:host", uri('news', [], 'host', 80, [], [], [])).
test(news2) :- uri_parse("news:host.subhost", uri('news', [], 'host.subhost', 80, [], [], [])).
test(news3) :- uri_parse("news:ho123st", uri('news', [], 'ho123st', 80, [], [], [])).
test(news4) :- uri_parse("news:", uri('news', [], [], 80, [], [], [])).
test(news5) :- uri_parse("news:ho st", uri('news', [], 'ho st', 80, [], [], [])).
test(news_1) :- \+(uri_parse("news:userinfo@host", _)).
test(news_2) :- \+(uri_parse("news:ho/st", _)).
test(news_3) :- \+(uri_parse("news:host/path", _)).
test(news_4) :- \+(uri_parse("news:host/path?query", _)).
test(news_5) :- \+(uri_parse("news:host/path?query#fragment", _)).
test(news_6) :- \+(uri_parse("news:host:80", _)).


% TEST SCHEMA ZOS
test(zos1) :- uri_parse("zos://host/id44(id8)", uri('zos', [], 'host', 80, 'id44(id8)', [], [])).
test(zos2) :- uri_parse("zos://userinfo@host/id44(id8)", uri('zos', 'userinfo', 'host', 80, 'id44(id8)', [], [])).
test(zos3) :- uri_parse("zos://userinfo@host:123/id44(id8)", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', [], [])).
test(zos4) :- uri_parse("zos://userinfo@host:123/id44(id8)?query", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', 'query', [])).
test(zos5) :- uri_parse("zos://userinfo@host:123/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', 'query', 'fragment')).
test(zos6) :- uri_parse("zos://host/id.44(id8)", uri('zos', [], 'host', 80, 'id.44(id8)', [], [])).
test(zos7) :- uri_parse("zos://host/i.d.4.4(id8)", uri('zos', [], 'host', 80, 'i.d.4.4(id8)', [], [])).
test(zos8) :- uri_parse("zos://host/i.d.4.4", uri('zos', [], 'host', 80, 'i.d.4.4', [], [])).
test(zos9) :- uri_parse("zos://host/id..prova", uri('zos', [], 'host', 80, 'id..prova', [], [])).
test(zos10) :- uri_parse("zos://host/id..prova(id8)", uri('zos', [], 'host', 80, 'id..prova(id8)', [], [])).
%test(zos11) :- uri_parse("zos://host/", _).
%test(zos12) :- uri_parse("zos://host", _).
test(zos13) :- uri_parse("zos:/id44(id8)", uri('zos', [], [], 80, 'id44(id8)', [], [])).
test(zos14) :- uri_parse("zos:/id44(id8)", uri('zos', [], [], 80, 'id44(id8)', [], [])).
test(zos15) :- uri_parse("zos:/id44(id8)", uri('zos', [], [], 80, 'id44(id8)', [], [])).
test(zos16) :- uri_parse("zos:/id44(id8)?query", uri('zos', [], [], 80, 'id44(id8)', 'query', [])).
test(zos17) :- uri_parse("zos:/id44(id8)?query#fragment", uri('zos', [], [], 80, 'id44(id8)', 'query', 'fragment')).
test(zos18) :- uri_parse("zos:/id.44(id8)", uri('zos', [], [], 80, 'id.44(id8)', [], [])).
test(zos19) :- uri_parse("zos:/i.d.4.4(id8)", uri('zos', [], [], 80, 'i.d.4.4(id8)', [], [])).
test(zos20) :- uri_parse("zos:/i.d.4.4", uri('zos', [], [], 80, 'i.d.4.4', [], [])).
test(zos21) :- uri_parse("zos:/id..prova", uri('zos', [], [], 80, 'id..prova', [], [])).
test(zos22) :- uri_parse("zos:/id..prova(id8)", uri('zos', [], [], 80, 'id..prova(id8)', [], [])).
%test(zos23) :- uri_parse("zos:/", _).
%test(zos24) :- uri_parse("zos:", _).
test(zos_1) :- \+(uri_parse("zos://host/id.(id8)", _)).
test(zos_2) :- \+(uri_parse("zos://host/.i.d", _)).
test(zos_3) :- \+(uri_parse("zos://host/.", _)).
test(zos_4) :- \+(uri_parse("zos://host/.(id8)", _)).
test(zos_5) :- \+(uri_parse("zos://host/a012345678901234567890123456789012345678901234(id)", _)).
test(zos_6) :- \+(uri_parse("zos://host/path(a012345678)", _)).
test(zos_7) :- \+(uri_parse("zos://host/a012345678901234567890123456789012345678901234(a012345678)", _)).
test(zos_8) :- \+(uri_parse("zos://host/(id44)", _)).
test(zos_9) :- \+(uri_parse("zos://host/pi@ppo(id44)", _)).
test(zos_10) :- \+(uri_parse("zos://host/pip po(id44)", _)).
test(zos_11) :- \+(uri_parse("zos://host/pippo.mar co(id44)", _)).
test(zos_12) :- \+(uri_parse("zos://host/pippo(id.44)", _)).
test(zos_13) :- \+(uri_parse("zos://host/pippo(id .44)", _)).
test(zos_14) :- \+(uri_parse("zos://host/pippo()", _)).
test(zos_15) :- \+(uri_parse("zos://host/pippo( )", _)).
test(zos_16) :- \+(uri_parse("zos://host/pippo(", _)).
test(zos_17) :- \+(uri_parse("zos://host/pippo)", _)).
test(zos_18) :- \+(uri_parse("zos://host/id.", _)).
test(zos_19) :- \+(uri_parse("zos://host/id(1)", _)).
test(zos_20) :- \+(uri_parse("zos://host/id(1id8)", _)).
test(zos_21) :- \+(uri_parse("zos://host/id..prova..", _)).
test(zos_22) :- \+(uri_parse("zos://host/..id..prova", _)).
test(zos_23) :- \+(uri_parse("zos://host/id..prova..(id8)", _)).
test(zos_24) :- \+(uri_parse("zos://host/..id..prova(id8)", _)).
test(zos_25) :- \+(uri_parse("zos://host/.i.d(id8)", _)).
test(zos_26) :- \+(uri_parse("zos:/id.(id8)", _)).
test(zos_27) :- \+(uri_parse("zos:/.i.d", _)).
test(zos_28) :- \+(uri_parse("zos:/.", _)).
test(zos_29) :- \+(uri_parse("zos:/.(id8)", _)).
test(zos_30) :- \+(uri_parse("zos:/a012345678901234567890123456789012345678901234(id)", _)).
test(zos_31) :- \+(uri_parse("zos:/path(a012345678)", _)).
test(zos_32) :- \+(uri_parse("zos:/a012345678901234567890123456789012345678901234(a012345678)", _)).
test(zos_33) :- \+(uri_parse("zos:/(id44)", _)).
test(zos_34) :- \+(uri_parse("zos:/pi@ppo(id44)", _)).
test(zos_35) :- \+(uri_parse("zos:/pip po(id44)", _)).
test(zos_36) :- \+(uri_parse("zos:/pippo.mar co(id44)", _)).
test(zos_37) :- \+(uri_parse("zos:/pippo(id.44)", _)).
test(zos_38) :- \+(uri_parse("zos:/pippo(id .44)", _)).
test(zos_39) :- \+(uri_parse("zos:/pippo()", _)).
test(zos_40) :- \+(uri_parse("zos:/pippo( )", _)).
test(zos_41) :- \+(uri_parse("zos:/pippo(", _)).
test(zos_42) :- \+(uri_parse("zos:/pippo)", _)).
test(zos_43) :- \+(uri_parse("zos:/id.", _)).
test(zos_44) :- \+(uri_parse("zos:/id(1)", _)).
test(zos_45) :- \+(uri_parse("zos:/id(1id8)", _)).
test(zos_46) :- \+(uri_parse("zos:/id..prova..", _)).
test(zos_47) :- \+(uri_parse("zos:/..id..prova", _)).
test(zos_48) :- \+(uri_parse("zos:/id..prova..(id8)", _)).
test(zos_49) :- \+(uri_parse("zos:/..id..prova(id8)", _)).
test(zos_50) :- \+(uri_parse("zos:/.i.d(id8)", _)).

% TEST INTEGRATION ---- URI-zos
test(uri_zos1) :- uri_parse("zos://userinfo@host.subhost:123/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host.subhost', 123, 'id44(id8)', 'query', 'fragment')).
test(uri_zos2) :- uri_parse("zos://userinfo@host.subhost:123/id44(id8)?query", uri('zos', 'userinfo', 'host.subhost', 123, 'id44(id8)', 'query', [])).
test(uri_zos3) :- uri_parse("zos://userinfo@host.subhost:123/id44(id8)#fragment", uri('zos', 'userinfo', 'host.subhost', 123, 'id44(id8)', [], 'fragment')).
test(uri_zos4) :- uri_parse("zos://userinfo@host.subhost:123/id44(id8)", uri('zos', 'userinfo', 'host.subhost', 123, 'id44(id8)', [], [])).
test(uri_zos5) :- uri_parse("zos://userinfo@host.subhost:123/id44?query#fragment", uri('zos', 'userinfo', 'host.subhost', 123, 'id44', 'query', 'fragment')).
test(uri_zos6) :- uri_parse("zos://userinfo@host.subhost:123/id44?query", uri('zos', 'userinfo', 'host.subhost', 123, 'id44', 'query', [])).
test(uri_zos7) :- uri_parse("zos://userinfo@host.subhost:123/id44#fragment", uri('zos', 'userinfo', 'host.subhost', 123, 'id44', [], 'fragment')).
test(uri_zos8) :- uri_parse("zos://userinfo@host.subhost:123/id44", uri('zos', 'userinfo', 'host.subhost', 123, 'id44', [], [])).
%test(uri_zos9) :- uri_parse("zos://userinfo@host.subhost:123/?query#fragment", uri('zos', 'userinfo', 'host.subhost', 123, [], 'query', 'fragment')).
%test(uri_zos10) :- uri_parse("zos://userinfo@host.subhost:123/?query", uri('zos', 'userinfo', 'host.subhost', 123, [], 'query', [])).
%test(uri_zos11) :- uri_parse("zos://userinfo@host.subhost:123/#fragment", uri('zos', 'userinfo', 'host.subhost', 123, [], [], 'fragment')).
%test(uri_zos12) :- uri_parse("zos://userinfo@host.subhost:123/", uri('zos', 'userinfo', 'host.subhost', 123, [], [], [])).
%test(uri_zos13) :- uri_parse("zos://userinfo@host.subhost:123", uri('zos', 'userinfo', 'host.subhost', 123, [], [], [])).

test(uri_zos14) :- uri_parse("zos://userinfo@host.subhost/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host.subhost', 80, 'id44(id8)', 'query', 'fragment')).
test(uri_zos15) :- uri_parse("zos://userinfo@host.subhost/id44(id8)?query", uri('zos', 'userinfo', 'host.subhost', 80, 'id44(id8)', 'query', [])).
test(uri_zos16) :- uri_parse("zos://userinfo@host.subhost/id44(id8)#fragment", uri('zos', 'userinfo', 'host.subhost', 80, 'id44(id8)', [], 'fragment')).
test(uri_zos17) :- uri_parse("zos://userinfo@host.subhost/id44(id8)", uri('zos', 'userinfo', 'host.subhost', 80, 'id44(id8)', [], [])).
test(uri_zos18) :- uri_parse("zos://userinfo@host.subhost/id44?query#fragment", uri('zos', 'userinfo', 'host.subhost', 80, 'id44', 'query', 'fragment')).
test(uri_zos19) :- uri_parse("zos://userinfo@host.subhost/id44?query", uri('zos', 'userinfo', 'host.subhost', 80, 'id44', 'query', [])).
test(uri_zos20) :- uri_parse("zos://userinfo@host.subhost/id44#fragment", uri('zos', 'userinfo', 'host.subhost', 80, 'id44', [], 'fragment')).
test(uri_zos21) :- uri_parse("zos://userinfo@host.subhost/id44", uri('zos', 'userinfo', 'host.subhost', 80, 'id44', [], [])).
%test(uri_zos22) :- uri_parse("zos://userinfo@host.subhost/?query#fragment", uri('zos', 'userinfo', 'host.subhost', 80, [], 'query', 'fragment')).
%test(uri_zos23) :- uri_parse("zos://userinfo@host.subhost/?query", uri('zos', 'userinfo', 'host.subhost', 80, [], 'query', [])).
%test(uri_zos24) :- uri_parse("zos://userinfo@host.subhost/#fragment", uri('zos', 'userinfo', 'host.subhost', 80, [], [], 'fragment')).
%test(uri_zos25) :- uri_parse("zos://userinfo@host.subhost/", uri('zos', 'userinfo', 'host.subhost', 80, [], [], [])).
%test(uri_zos26) :- uri_parse("zos://userinfo@host.subhost", uri('zos', 'userinfo', 'host.subhost', 80, [], [], [])).

test(uri_zos27) :- uri_parse("zos://host.subhost:123/id44(id8)?query#fragment", uri('zos', [], 'host.subhost', 123, 'id44(id8)', 'query', 'fragment')).
test(uri_zos28) :- uri_parse("zos://host.subhost:123/id44(id8)?query", uri('zos', [], 'host.subhost', 123, 'id44(id8)', 'query', [])).
test(uri_zos29) :- uri_parse("zos://host.subhost:123/id44(id8)#fragment", uri('zos', [], 'host.subhost', 123, 'id44(id8)', [], 'fragment')).
test(uri_zos30) :- uri_parse("zos://host.subhost:123/id44(id8)", uri('zos', [], 'host.subhost', 123, 'id44(id8)', [], [])).
test(uri_zos31) :- uri_parse("zos://host.subhost:123/id44?query#fragment", uri('zos', [], 'host.subhost', 123, 'id44', 'query', 'fragment')).
test(uri_zos32) :- uri_parse("zos://host.subhost:123/id44?query", uri('zos', [], 'host.subhost', 123, 'id44', 'query', [])).
test(uri_zos33) :- uri_parse("zos://host.subhost:123/id44#fragment", uri('zos', [], 'host.subhost', 123, 'id44', [], 'fragment')).
test(uri_zos34) :- uri_parse("zos://host.subhost:123/id44", uri('zos', [], 'host.subhost', 123, 'id44', [], [])).
%test(uri_zos35) :- uri_parse("zos://host.subhost:123/?query#fragment", uri('zos', [], 'host.subhost', 123, [], 'query', 'fragment')).
%test(uri_zos36) :- uri_parse("zos://host.subhost:123/?query", uri('zos', [], 'host.subhost', 123, [], 'query', [])).
%test(uri_zos37) :- uri_parse("zos://host.subhost:123/#fragment", uri('zos', [], 'host.subhost', 123, [], [], 'fragment')).
%test(uri_zos38) :- uri_parse("zos://host.subhost:123/", uri('zos', [], 'host.subhost', 123, [], [], [])).
%test(uri_zos39) :- uri_parse("zos://host.subhost:123", uri('zos', [], 'host.subhost', 123, [], [], [])).

test(uri_zos40) :- uri_parse("zos://host.subhost/id44(id8)?query#fragment", uri('zos', [], 'host.subhost', 80, 'id44(id8)', 'query', 'fragment')).
test(uri_zos41) :- uri_parse("zos://host.subhost/id44(id8)?query", uri('zos', [], 'host.subhost', 80, 'id44(id8)', 'query', [])).
test(uri_zos42) :- uri_parse("zos://host.subhost/id44(id8)#fragment", uri('zos', [], 'host.subhost', 80, 'id44(id8)', [], 'fragment')).
test(uri_zos43) :- uri_parse("zos://host.subhost/id44(id8)", uri('zos', [], 'host.subhost', 80, 'id44(id8)', [], [])).
test(uri_zos44) :- uri_parse("zos://host.subhost/id44?query#fragment", uri('zos', [], 'host.subhost', 80, 'id44', 'query', 'fragment')).
test(uri_zos45) :- uri_parse("zos://host.subhost/id44?query", uri('zos', [], 'host.subhost', 80, 'id44', 'query', [])).
test(uri_zos46) :- uri_parse("zos://host.subhost/id44#fragment", uri('zos', [], 'host.subhost', 80, 'id44', [], 'fragment')).
test(uri_zos47) :- uri_parse("zos://host.subhost/id44", uri('zos', [], 'host.subhost', 80, 'id44', [], [])).
%test(uri_zos48) :- uri_parse("zos://host.subhost/?query#fragment", uri('zos', [], 'host.subhost', 80, [], 'query', 'fragment')).
%test(uri_zos49) :- uri_parse("zos://host.subhost/?query", uri('zos', [], 'host.subhost', 80, [], 'query', [])).
%test(uri_zos50) :- uri_parse("zos://host.subhost/#fragment", uri('zos', [], 'host.subhost', 80, [], [], 'fragment')).
%test(uri_zos51) :- uri_parse("zos://host.subhost/", uri('zos', [], 'host.subhost', 80, [], [], [])).
%test(uri_zos52) :- uri_parse("zos://host.subhost", uri('zos', [], 'host.subhost', 80, [], [], [])).

test(uri_zos53) :- uri_parse("zos://userinfo@host:123/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', 'query', 'fragment')).
test(uri_zos54) :- uri_parse("zos://userinfo@host:123/id44(id8)?query", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', 'query', [])).
test(uri_zos55) :- uri_parse("zos://userinfo@host:123/id44(id8)#fragment", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', [], 'fragment')).
test(uri_zos56) :- uri_parse("zos://userinfo@host:123/id44(id8)", uri('zos', 'userinfo', 'host', 123, 'id44(id8)', [], [])).
test(uri_zos57) :- uri_parse("zos://userinfo@host:123/id44?query#fragment", uri('zos', 'userinfo', 'host', 123, 'id44', 'query', 'fragment')).
test(uri_zos58) :- uri_parse("zos://userinfo@host:123/id44?query", uri('zos', 'userinfo', 'host', 123, 'id44', 'query', [])).
test(uri_zos59) :- uri_parse("zos://userinfo@host:123/id44#fragment", uri('zos', 'userinfo', 'host', 123, 'id44', [], 'fragment')).
test(uri_zos60) :- uri_parse("zos://userinfo@host:123/id44", uri('zos', 'userinfo', 'host', 123, 'id44', [], [])).
%test(uri_zos61) :- uri_parse("zos://userinfo@host:123/?query#fragment", uri('zos', 'userinfo', 'host', 123, [], 'query', 'fragment')).
%test(uri_zos62) :- uri_parse("zos://userinfo@host:123/?query", uri('zos', 'userinfo', 'host', 123, [], 'query', [])).
%test(uri_zos63) :- uri_parse("zos://userinfo@host:123/#fragment", uri('zos', 'userinfo', 'host', 123, [], [], 'fragment')).
%test(uri_zos64) :- uri_parse("zos://userinfo@host:123/", uri('zos', 'userinfo', 'host', 123, [], [], [])).
%test(uri_zos65) :- uri_parse("zos://userinfo@host:123", uri('zos', 'userinfo', 'host', 123, [], [], [])).

test(uri_zos66) :- uri_parse("zos://userinfo@host/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host', 80, 'id44(id8)', 'query', 'fragment')).
test(uri_zos67) :- uri_parse("zos://userinfo@host/id44(id8)?query", uri('zos', 'userinfo', 'host', 80, 'id44(id8)', 'query', [])).
test(uri_zos68) :- uri_parse("zos://userinfo@host/id44(id8)#fragment", uri('zos', 'userinfo', 'host', 80, 'id44(id8)', [], 'fragment')).
test(uri_zos69) :- uri_parse("zos://userinfo@host/id44(id8)", uri('zos', 'userinfo', 'host', 80, 'id44(id8)', [], [])).
test(uri_zos70) :- uri_parse("zos://userinfo@host/id44?query#fragment", uri('zos', 'userinfo', 'host', 80, 'id44', 'query', 'fragment')).
test(uri_zos71) :- uri_parse("zos://userinfo@host/id44?query", uri('zos', 'userinfo', 'host', 80, 'id44', 'query', [])).
test(uri_zos72) :- uri_parse("zos://userinfo@host/id44#fragment", uri('zos', 'userinfo', 'host', 80, 'id44', [], 'fragment')).
test(uri_zos73) :- uri_parse("zos://userinfo@host/id44", uri('zos', 'userinfo', 'host', 80, 'id44', [], [])).
%test(uri_zos74) :- uri_parse("zos://userinfo@host/?query#fragment", uri('zos', 'userinfo', 'host', 80, [], 'query', 'fragment')).
%test(uri_zos75) :- uri_parse("zos://userinfo@host/?query", uri('zos', 'userinfo', 'host', 80, [], 'query', [])).
%test(uri_zos76) :- uri_parse("zos://userinfo@host/#fragment", uri('zos', 'userinfo', 'host', 80, [], [], 'fragment')).
%test(uri_zos77) :- uri_parse("zos://userinfo@host/", uri('zos', 'userinfo', 'host', 80, [], [], [])).
%test(uri_zos78) :- uri_parse("zos://userinfo@host", uri('zos', 'userinfo', 'host', 80, [], [], [])).

test(uri_zos79) :- uri_parse("zos://host:123/id44(id8)?query#fragment", uri('zos', [], 'host', 123, 'id44(id8)', 'query', 'fragment')).
test(uri_zos80) :- uri_parse("zos://host:123/id44(id8)?query", uri('zos', [], 'host', 123, 'id44(id8)', 'query', [])).
test(uri_zos81) :- uri_parse("zos://host:123/id44(id8)#fragment", uri('zos', [], 'host', 123, 'id44(id8)', [], 'fragment')).
test(uri_zos82) :- uri_parse("zos://host:123/id44(id8)", uri('zos', [], 'host', 123, 'id44(id8)', [], [])).
test(uri_zos83) :- uri_parse("zos://host:123/id44?query#fragment", uri('zos', [], 'host', 123, 'id44', 'query', 'fragment')).
test(uri_zos84) :- uri_parse("zos://host:123/id44?query", uri('zos', [], 'host', 123, 'id44', 'query', [])).
test(uri_zos85) :- uri_parse("zos://host:123/id44#fragment", uri('zos', [], 'host', 123, 'id44', [], 'fragment')).
test(uri_zos86) :- uri_parse("zos://host:123/id44", uri('zos', [], 'host', 123, 'id44', [], [])).
%test(uri_zos87) :- uri_parse("zos://host:123/?query#fragment", uri('zos', [], 'host', 123, [], 'query', 'fragment')).
%test(uri_zos88) :- uri_parse("zos://host:123/?query", uri('zos', [], 'host', 123, [], 'query', [])).
%test(uri_zos89) :- uri_parse("zos://host:123/#fragment", uri('zos', [], 'host', 123, [], [], 'fragment')).
%test(uri_zos90) :- uri_parse("zos://host:123/", uri('zos', [], 'host', 123, [], [], [])).
%test(uri_zos91) :- uri_parse("zos://host:123", uri('zos', [], 'host', 123, [], [], [])).

test(uri_zos92) :- uri_parse("zos://host/id44(id8)?query#fragment", uri('zos', [], 'host', 80, 'id44(id8)', 'query', 'fragment')).
test(uri_zos93) :- uri_parse("zos://host/id44(id8)?query", uri('zos', [], 'host', 80, 'id44(id8)', 'query', [])).
test(uri_zos94) :- uri_parse("zos://host/id44(id8)#fragment", uri('zos', [], 'host', 80, 'id44(id8)', [], 'fragment')).
test(uri_zos95) :- uri_parse("zos://host/id44(id8)", uri('zos', [], 'host', 80, 'id44(id8)', [], [])).
test(uri_zos96) :- uri_parse("zos://host/id44?query#fragment", uri('zos', [], 'host', 80, 'id44', 'query', 'fragment')).
test(uri_zos97) :- uri_parse("zos://host/id44?query", uri('zos', [], 'host', 80, 'id44', 'query', [])).
test(uri_zos98) :- uri_parse("zos://host/id44#fragment", uri('zos', [], 'host', 80, 'id44', [], 'fragment')).
test(uri_zos99) :- uri_parse("zos://host/id44", uri('zos', [], 'host', 80, 'id44', [], [])).
%test(uri_zos100) :- uri_parse("zos://host/?query#fragment", uri('zos', [], 'host', 80, [], 'query', 'fragment')).
%test(uri_zos101) :- uri_parse("zos://host/?query", uri('zos', [], 'host', 80, [], 'query', [])).
%test(uri_zos102) :- uri_parse("zos://host/#fragment", uri('zos', [], 'host', 80, [], [], 'fragment')).
%test(uri_zos103) :- uri_parse("zos://host/", uri('zos', [], 'host', 80, [], [], [])).
%test(uri_zos104) :- uri_parse("zos://host", uri('zos', [], 'host', 80, [], [], [])).

test(uri_zos105) :- uri_parse("zos:/id44(id8)?query#fragment", uri('zos', [], [], 80, 'id44(id8)', 'query', 'fragment')).
test(uri_zos106) :- uri_parse("zos:/id44(id8)?query", uri('zos', [], [], 80, 'id44(id8)', 'query', [])).
test(uri_zos107) :- uri_parse("zos:/id44(id8)#fragment", uri('zos', [], [], 80, 'id44(id8)', [], 'fragment')).
test(uri_zos108) :- uri_parse("zos:/id44(id8)", uri('zos', [], [], 80, 'id44(id8)', [], [])).
test(uri_zos109) :- uri_parse("zos:/id44?query#fragment", uri('zos', [], [], 80, 'id44', 'query', 'fragment')).
test(uri_zos110) :- uri_parse("zos:/id44?query", uri('zos', [], [], 80, 'id44', 'query', [])).
test(uri_zos111) :- uri_parse("zos:/id44#fragment", uri('zos', [], [], 80, 'id44', [], 'fragment')).
test(uri_zos112) :- uri_parse("zos:/id44", uri('zos', [], [], 80, 'id44', [], [])).
%test(uri_zos113) :- uri_parse("zos:/?query#fragment", uri('zos', [], [], 80, [], 'query', 'fragment')).
%test(uri_zos114) :- uri_parse("zos:/?query", uri('zos', [], [], 80, [], 'query', [])).
%test(uri_zos115) :- uri_parse("zos:/#fragment", uri('zos', [], [], 80, [], [], 'fragment')).
%test(uri_zos116) :- uri_parse("zos:/", uri('zos', [], [], 80, [], [], [])).
%test(uri_zos117) :- uri_parse("zos:", uri('zos', [], [], 80, [], [], [])).

test(uri_zos_1) :- \+(uri_parse("zos://", _)).
test(uri_zos_2) :- \+(uri_parse("zos:///id44(id8)?query#fragment", _)).
test(uri_zos_3) :- \+(uri_parse("zos:///id44(id8)?query", _)).
test(uri_zos_4) :- \+(uri_parse("zos:///id44(id8)#fragment", _)).
test(uri_zos_5) :- \+(uri_parse("zos:///id44(id8)", _)).
test(uri_zos_6) :- \+(uri_parse("zos:///id44?query#fragment", _)).
test(uri_zos_7) :- \+(uri_parse("zos:///id44?query", _)).
test(uri_zos_8) :- \+(uri_parse("zos:///id44#fragment", _)).
test(uri_zos_9) :- \+(uri_parse("zos:///id44", _)).
test(uri_zos_10) :- \+(uri_parse("zos:///?query#fragment", _)).
test(uri_zos_11) :- \+(uri_parse("zos:///?query", _)).
test(uri_zos_12) :- \+(uri_parse("zos:///#fragment", _)).
test(uri_zos_13) :- \+(uri_parse("zos:///", _)).
test(uri_zos_14) :- \+(uri_parse("zos://", _)).
test(uri_zos_15) :- \+(uri_parse("zos://host?query#fragment", _)).
test(uri_zos_16) :- \+(uri_parse("zos://host?query", _)).
test(uri_zos_17) :- \+(uri_parse("zos://host#fragment", _)).
test(uri_zos_18) :- \+(uri_parse("://host/id44(id8)?query#fragment", _)).
test(uri_zos_19) :- \+(uri_parse("://host/id44(id8)?query", _)).
test(uri_zos_20) :- \+(uri_parse("://host/id44(id8)#fragment", _)).
test(uri_zos_21) :- \+(uri_parse("://host/id44(id8)", _)).
test(uri_zos_22) :- \+(uri_parse("://host/id44?query#fragment", _)).
test(uri_zos_23) :- \+(uri_parse("://host/id44?query", _)).
test(uri_zos_24) :- \+(uri_parse("://host/id44#fragment", _)).
test(uri_zos_25) :- \+(uri_parse("://host/id44", _)).
test(uri_zos_26) :- \+(uri_parse("://host/?query#fragment", _)).
test(uri_zos_27) :- \+(uri_parse("://host/?query", _)).
test(uri_zos_28) :- \+(uri_parse("://host/#fragment", _)).
test(uri_zos_29) :- \+(uri_parse("://host/", _)).
test(uri_zos_30) :- \+(uri_parse("://host", _)).
test(uri_zos_31) :- \+(uri_parse("zos//host/id44(id8)?query#fragment", _)).
test(uri_zos_32) :- \+(uri_parse("zos//host/id44(id8)?query", _)).
test(uri_zos_33) :- \+(uri_parse("zos//host/id44(id8)#fragment", _)).
test(uri_zos_34) :- \+(uri_parse("zos//host/id44(id8)", _)).
test(uri_zos_35) :- \+(uri_parse("zos//host/id44?query#fragment", _)).
test(uri_zos_36) :- \+(uri_parse("zos//host/id44?query", _)).
test(uri_zos_37) :- \+(uri_parse("zos//host/id44#fragment", _)).
test(uri_zos_38) :- \+(uri_parse("zos//host/id44", _)).
test(uri_zos_39) :- \+(uri_parse("zos//host/?query#fragment", _)).
test(uri_zos_40) :- \+(uri_parse("zos//host/?query", _)).
test(uri_zos_41) :- \+(uri_parse("zos//host/#fragment", _)).
test(uri_zos_42) :- \+(uri_parse("zos//host/", _)).
test(uri_zos_43) :- \+(uri_parse("zos//host", _)).

test(uri_zos_44) :- \+(uri_parse("zos:id44(id8)?query#fragment", _)).
test(uri_zos_45) :- \+(uri_parse("zos:id44(id8)?query", _)).
test(uri_zos_46) :- \+(uri_parse("zos:id44(id8)#fragment", _)).
test(uri_zos_47) :- \+(uri_parse("zos:id44(id8)", _)).
test(uri_zos_48) :- \+(uri_parse("zos:id44?query#fragment", _)).
test(uri_zos_49) :- \+(uri_parse("zos:id44?query", _)).
test(uri_zos_50) :- \+(uri_parse("zos:id44#fragment", _)).
test(uri_zos_51) :- \+(uri_parse("zos:id44", _)).
test(uri_zos_52) :- \+(uri_parse("zos:?query#fragment", _)).
test(uri_zos_53) :- \+(uri_parse("zos:#fragment", _)).
test(uri_zos_54) :- \+(uri_parse("zos:?query", _)).

test(uri_zos_55) :- \+(uri_parse("zos/id44(id8)?query#fragment", _)).
test(uri_zos_56) :- \+(uri_parse("zos/id44(id8)?query", _)).
test(uri_zos_57) :- \+(uri_parse("zos/id44(id8)#fragment", _)).
test(uri_zos_58) :- \+(uri_parse("zos/id44(id8)", _)).
test(uri_zos_59) :- \+(uri_parse("zos/id44?query#fragment", _)).
test(uri_zos_60) :- \+(uri_parse("zos/id44?query", _)).
test(uri_zos_61) :- \+(uri_parse("zos/id44#fragment", _)).
test(uri_zos_62) :- \+(uri_parse("zos/id44", _)).
test(uri_zos_63) :- \+(uri_parse("zos/?query#fragment", _)).
test(uri_zos_64) :- \+(uri_parse("zos/#fragment", _)).
test(uri_zos_65) :- \+(uri_parse("zos/?query", _)).
test(uri_zos_66) :- \+(uri_parse("zos/", _)).

test(uri_zos_67) :- \+(uri_parse("httppath(id8)?query#fragment", _)).
test(uri_zos_68) :- \+(uri_parse("httppath(id8)?query", _)).
test(uri_zos_69) :- \+(uri_parse("httppath(id8)#fragment", _)).
test(uri_zos_70) :- \+(uri_parse("httppath(id8)", _)).
test(uri_zos_71) :- \+(uri_parse("httppath?query#fragment", _)).
test(uri_zos_72) :- \+(uri_parse("httppath?query", _)).
test(uri_zos_73) :- \+(uri_parse("httppath#fragment", _)).
test(uri_zos_74) :- \+(uri_parse("httppath", _)).
test(uri_zos_75) :- \+(uri_parse("zos?query#fragment", _)).
test(uri_zos_76) :- \+(uri_parse("zos#fragment", _)).
test(uri_zos_77) :- \+(uri_parse("zos?query", _)).
test(uri_zos_78) :- \+(uri_parse("zos", _)).

test(uri_zos_79) :- \+(uri_parse(":/id44(id8)?query#fragment", _)).
test(uri_zos_80) :- \+(uri_parse(":/id44(id8)?query", _)).
test(uri_zos_81) :- \+(uri_parse(":/id44(id8)#fragment", _)).
test(uri_zos_82) :- \+(uri_parse(":/id44(id8)", _)).
test(uri_zos_83) :- \+(uri_parse(":/id44?query#fragment", _)).
test(uri_zos_84) :- \+(uri_parse(":/id44?query", _)).
test(uri_zos_85) :- \+(uri_parse(":/id44#fragment", _)).
test(uri_zos_86) :- \+(uri_parse(":/id44", _)).
test(uri_zos_87) :- \+(uri_parse(":/?query#fragment", _)).
test(uri_zos_88) :- \+(uri_parse(":/#fragment", _)).
test(uri_zos_89) :- \+(uri_parse(":/?query", _)).
test(uri_zos_90) :- \+(uri_parse(":/", _)).

test(uri_zos_91) :- \+(uri_parse(":id44(id8)?query#fragment", _)).
test(uri_zos_92) :- \+(uri_parse(":id44(id8)?query", _)).
test(uri_zos_93) :- \+(uri_parse(":id44(id8)#fragment", _)).
test(uri_zos_94) :- \+(uri_parse(":id44(id8)", _)).
test(uri_zos_95) :- \+(uri_parse(":id44?query#fragment", _)).
test(uri_zos_96) :- \+(uri_parse(":id44?query", _)).
test(uri_zos_97) :- \+(uri_parse(":id44#fragment", _)).
test(uri_zos_98) :- \+(uri_parse(":id44", _)).
test(uri_zos_99) :- \+(uri_parse(":?query#fragment", _)).
test(uri_zos_100) :- \+(uri_parse(":#fragment", _)).
test(uri_zos_101) :- \+(uri_parse(":?query", _)).
test(uri_zos_102) :- \+(uri_parse(":", _)).

% TEST INTEGRATION ---- URI
test(uri1) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', 'query', 'fragment')).
test(uri2) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', 'query', [])).
test(uri3) :- uri_parse("http://userinfo@host.subhost:123/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', [], 'fragment')).
test(uri4) :- uri_parse("http://userinfo@host.subhost:123/path/subpath", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', [], [])).
test(uri5) :- uri_parse("http://userinfo@host.subhost:123/path?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path', 'query', 'fragment')).
test(uri6) :- uri_parse("http://userinfo@host.subhost:123/path?query", uri('http', 'userinfo', 'host.subhost', 123, 'path', 'query', [])).
test(uri7) :- uri_parse("http://userinfo@host.subhost:123/path#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path', [], 'fragment')).
test(uri8) :- uri_parse("http://userinfo@host.subhost:123/path", uri('http', 'userinfo', 'host.subhost', 123, 'path', [], [])).
test(uri9) :- uri_parse("http://userinfo@host.subhost:123/?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, [], 'query', 'fragment')).
test(uri10) :- uri_parse("http://userinfo@host.subhost:123/?query", uri('http', 'userinfo', 'host.subhost', 123, [], 'query', [])).
test(uri11) :- uri_parse("http://userinfo@host.subhost:123/#fragment", uri('http', 'userinfo', 'host.subhost', 123, [], [], 'fragment')).
test(uri12) :- uri_parse("http://userinfo@host.subhost:123/", uri('http', 'userinfo', 'host.subhost', 123, [], [], [])).
test(uri13) :- uri_parse("http://userinfo@host.subhost:123", uri('http', 'userinfo', 'host.subhost', 123, [], [], [])).

test(uri14) :- uri_parse("http://userinfo@host.subhost/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', 'query', 'fragment')).
test(uri15) :- uri_parse("http://userinfo@host.subhost/path/subpath?query", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', 'query', [])).
test(uri16) :- uri_parse("http://userinfo@host.subhost/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', [], 'fragment')).
test(uri17) :- uri_parse("http://userinfo@host.subhost/path/subpath", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', [], [])).
test(uri18) :- uri_parse("http://userinfo@host.subhost/path?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path', 'query', 'fragment')).
test(uri19) :- uri_parse("http://userinfo@host.subhost/path?query", uri('http', 'userinfo', 'host.subhost', 80, 'path', 'query', [])).
test(uri20) :- uri_parse("http://userinfo@host.subhost/path#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path', [], 'fragment')).
test(uri21) :- uri_parse("http://userinfo@host.subhost/path", uri('http', 'userinfo', 'host.subhost', 80, 'path', [], [])).
test(uri22) :- uri_parse("http://userinfo@host.subhost/?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, [], 'query', 'fragment')).
test(uri23) :- uri_parse("http://userinfo@host.subhost/?query", uri('http', 'userinfo', 'host.subhost', 80, [], 'query', [])).
test(uri24) :- uri_parse("http://userinfo@host.subhost/#fragment", uri('http', 'userinfo', 'host.subhost', 80, [], [], 'fragment')).
test(uri25) :- uri_parse("http://userinfo@host.subhost/", uri('http', 'userinfo', 'host.subhost', 80, [], [], [])).
test(uri26) :- uri_parse("http://userinfo@host.subhost", uri('http', 'userinfo', 'host.subhost', 80, [], [], [])).

test(uri27) :- uri_parse("http://host.subhost:123/path/subpath?query#fragment", uri('http', [], 'host.subhost', 123, 'path/subpath', 'query', 'fragment')).
test(uri28) :- uri_parse("http://host.subhost:123/path/subpath?query", uri('http', [], 'host.subhost', 123, 'path/subpath', 'query', [])).
test(uri29) :- uri_parse("http://host.subhost:123/path/subpath#fragment", uri('http', [], 'host.subhost', 123, 'path/subpath', [], 'fragment')).
test(uri30) :- uri_parse("http://host.subhost:123/path/subpath", uri('http', [], 'host.subhost', 123, 'path/subpath', [], [])).
test(uri31) :- uri_parse("http://host.subhost:123/path?query#fragment", uri('http', [], 'host.subhost', 123, 'path', 'query', 'fragment')).
test(uri32) :- uri_parse("http://host.subhost:123/path?query", uri('http', [], 'host.subhost', 123, 'path', 'query', [])).
test(uri33) :- uri_parse("http://host.subhost:123/path#fragment", uri('http', [], 'host.subhost', 123, 'path', [], 'fragment')).
test(uri34) :- uri_parse("http://host.subhost:123/path", uri('http', [], 'host.subhost', 123, 'path', [], [])).
test(uri35) :- uri_parse("http://host.subhost:123/?query#fragment", uri('http', [], 'host.subhost', 123, [], 'query', 'fragment')).
test(uri36) :- uri_parse("http://host.subhost:123/?query", uri('http', [], 'host.subhost', 123, [], 'query', [])).
test(uri37) :- uri_parse("http://host.subhost:123/#fragment", uri('http', [], 'host.subhost', 123, [], [], 'fragment')).
test(uri38) :- uri_parse("http://host.subhost:123/", uri('http', [], 'host.subhost', 123, [], [], [])).
test(uri39) :- uri_parse("http://host.subhost:123", uri('http', [], 'host.subhost', 123, [], [], [])).

test(uri40) :- uri_parse("http://host.subhost/path/subpath?query#fragment", uri('http', [], 'host.subhost', 80, 'path/subpath', 'query', 'fragment')).
test(uri41) :- uri_parse("http://host.subhost/path/subpath?query", uri('http', [], 'host.subhost', 80, 'path/subpath', 'query', [])).
test(uri42) :- uri_parse("http://host.subhost/path/subpath#fragment", uri('http', [], 'host.subhost', 80, 'path/subpath', [], 'fragment')).
test(uri43) :- uri_parse("http://host.subhost/path/subpath", uri('http', [], 'host.subhost', 80, 'path/subpath', [], [])).
test(uri44) :- uri_parse("http://host.subhost/path?query#fragment", uri('http', [], 'host.subhost', 80, 'path', 'query', 'fragment')).
test(uri45) :- uri_parse("http://host.subhost/path?query", uri('http', [], 'host.subhost', 80, 'path', 'query', [])).
test(uri46) :- uri_parse("http://host.subhost/path#fragment", uri('http', [], 'host.subhost', 80, 'path', [], 'fragment')).
test(uri47) :- uri_parse("http://host.subhost/path", uri('http', [], 'host.subhost', 80, 'path', [], [])).
test(uri48) :- uri_parse("http://host.subhost/?query#fragment", uri('http', [], 'host.subhost', 80, [], 'query', 'fragment')).
test(uri49) :- uri_parse("http://host.subhost/?query", uri('http', [], 'host.subhost', 80, [], 'query', [])).
test(uri50) :- uri_parse("http://host.subhost/#fragment", uri('http', [], 'host.subhost', 80, [], [], 'fragment')).
test(uri51) :- uri_parse("http://host.subhost/", uri('http', [], 'host.subhost', 80, [], [], [])).
test(uri52) :- uri_parse("http://host.subhost", uri('http', [], 'host.subhost', 80, [], [], [])).

test(uri53) :- uri_parse("http://userinfo@host:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host', 123, 'path/subpath', 'query', 'fragment')).
test(uri54) :- uri_parse("http://userinfo@host:123/path/subpath?query", uri('http', 'userinfo', 'host', 123, 'path/subpath', 'query', [])).
test(uri55) :- uri_parse("http://userinfo@host:123/path/subpath#fragment", uri('http', 'userinfo', 'host', 123, 'path/subpath', [], 'fragment')).
test(uri56) :- uri_parse("http://userinfo@host:123/path/subpath", uri('http', 'userinfo', 'host', 123, 'path/subpath', [], [])).
test(uri57) :- uri_parse("http://userinfo@host:123/path?query#fragment", uri('http', 'userinfo', 'host', 123, 'path', 'query', 'fragment')).
test(uri58) :- uri_parse("http://userinfo@host:123/path?query", uri('http', 'userinfo', 'host', 123, 'path', 'query', [])).
test(uri59) :- uri_parse("http://userinfo@host:123/path#fragment", uri('http', 'userinfo', 'host', 123, 'path', [], 'fragment')).
test(uri60) :- uri_parse("http://userinfo@host:123/path", uri('http', 'userinfo', 'host', 123, 'path', [], [])).
test(uri61) :- uri_parse("http://userinfo@host:123/?query#fragment", uri('http', 'userinfo', 'host', 123, [], 'query', 'fragment')).
test(uri62) :- uri_parse("http://userinfo@host:123/?query", uri('http', 'userinfo', 'host', 123, [], 'query', [])).
test(uri63) :- uri_parse("http://userinfo@host:123/#fragment", uri('http', 'userinfo', 'host', 123, [], [], 'fragment')).
test(uri64) :- uri_parse("http://userinfo@host:123/", uri('http', 'userinfo', 'host', 123, [], [], [])).
test(uri65) :- uri_parse("http://userinfo@host:123", uri('http', 'userinfo', 'host', 123, [], [], [])).

test(uri66) :- uri_parse("http://userinfo@host/path/subpath?query#fragment", uri('http', 'userinfo', 'host', 80, 'path/subpath', 'query', 'fragment')).
test(uri67) :- uri_parse("http://userinfo@host/path/subpath?query", uri('http', 'userinfo', 'host', 80, 'path/subpath', 'query', [])).
test(uri68) :- uri_parse("http://userinfo@host/path/subpath#fragment", uri('http', 'userinfo', 'host', 80, 'path/subpath', [], 'fragment')).
test(uri69) :- uri_parse("http://userinfo@host/path/subpath", uri('http', 'userinfo', 'host', 80, 'path/subpath', [], [])).
test(uri70) :- uri_parse("http://userinfo@host/path?query#fragment", uri('http', 'userinfo', 'host', 80, 'path', 'query', 'fragment')).
test(uri71) :- uri_parse("http://userinfo@host/path?query", uri('http', 'userinfo', 'host', 80, 'path', 'query', [])).
test(uri72) :- uri_parse("http://userinfo@host/path#fragment", uri('http', 'userinfo', 'host', 80, 'path', [], 'fragment')).
test(uri73) :- uri_parse("http://userinfo@host/path", uri('http', 'userinfo', 'host', 80, 'path', [], [])).
test(uri74) :- uri_parse("http://userinfo@host/?query#fragment", uri('http', 'userinfo', 'host', 80, [], 'query', 'fragment')).
test(uri75) :- uri_parse("http://userinfo@host/?query", uri('http', 'userinfo', 'host', 80, [], 'query', [])).
test(uri76) :- uri_parse("http://userinfo@host/#fragment", uri('http', 'userinfo', 'host', 80, [], [], 'fragment')).
test(uri77) :- uri_parse("http://userinfo@host/", uri('http', 'userinfo', 'host', 80, [], [], [])).
test(uri78) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', 80, [], [], [])).

test(uri79) :- uri_parse("http://host:123/path/subpath?query#fragment", uri('http', [], 'host', 123, 'path/subpath', 'query', 'fragment')).
test(uri80) :- uri_parse("http://host:123/path/subpath?query", uri('http', [], 'host', 123, 'path/subpath', 'query', [])).
test(uri81) :- uri_parse("http://host:123/path/subpath#fragment", uri('http', [], 'host', 123, 'path/subpath', [], 'fragment')).
test(uri82) :- uri_parse("http://host:123/path/subpath", uri('http', [], 'host', 123, 'path/subpath', [], [])).
test(uri83) :- uri_parse("http://host:123/path?query#fragment", uri('http', [], 'host', 123, 'path', 'query', 'fragment')).
test(uri84) :- uri_parse("http://host:123/path?query", uri('http', [], 'host', 123, 'path', 'query', [])).
test(uri85) :- uri_parse("http://host:123/path#fragment", uri('http', [], 'host', 123, 'path', [], 'fragment')).
test(uri86) :- uri_parse("http://host:123/path", uri('http', [], 'host', 123, 'path', [], [])).
test(uri87) :- uri_parse("http://host:123/?query#fragment", uri('http', [], 'host', 123, [], 'query', 'fragment')).
test(uri88) :- uri_parse("http://host:123/?query", uri('http', [], 'host', 123, [], 'query', [])).
test(uri89) :- uri_parse("http://host:123/#fragment", uri('http', [], 'host', 123, [], [], 'fragment')).
test(uri90) :- uri_parse("http://host:123/", uri('http', [], 'host', 123, [], [], [])).
test(uri91) :- uri_parse("http://host:123", uri('http', [], 'host', 123, [], [], [])).

test(uri92) :- uri_parse("http://host/path/subpath?query#fragment", uri('http', [], 'host', 80, 'path/subpath', 'query', 'fragment')).
test(uri93) :- uri_parse("http://host/path/subpath?query", uri('http', [], 'host', 80, 'path/subpath', 'query', [])).
test(uri94) :- uri_parse("http://host/path/subpath#fragment", uri('http', [], 'host', 80, 'path/subpath', [], 'fragment')).
test(uri95) :- uri_parse("http://host/path/subpath", uri('http', [], 'host', 80, 'path/subpath', [], [])).
test(uri96) :- uri_parse("http://host/path?query#fragment", uri('http', [], 'host', 80, 'path', 'query', 'fragment')).
test(uri97) :- uri_parse("http://host/path?query", uri('http', [], 'host', 80, 'path', 'query', [])).
test(uri98) :- uri_parse("http://host/path#fragment", uri('http', [], 'host', 80, 'path', [], 'fragment')).
test(uri99) :- uri_parse("http://host/path", uri('http', [], 'host', 80, 'path', [], [])).
test(uri100) :- uri_parse("http://host/?query#fragment", uri('http', [], 'host', 80, [], 'query', 'fragment')).
test(uri101) :- uri_parse("http://host/?query", uri('http', [], 'host', 80, [], 'query', [])).
test(uri102) :- uri_parse("http://host/#fragment", uri('http', [], 'host', 80, [], [], 'fragment')).
test(uri103) :- uri_parse("http://host/", uri('http', [], 'host', 80, [], [], [])).
test(uri104) :- uri_parse("http://host", uri('http', [], 'host', 80, [], [], [])).

test(uri105) :- uri_parse("http:/path/subpath?query#fragment", uri('http', [], [], 80, 'path/subpath', 'query', 'fragment')).
test(uri106) :- uri_parse("http:/path/subpath?query", uri('http', [], [], 80, 'path/subpath', 'query', [])).
test(uri107) :- uri_parse("http:/path/subpath#fragment", uri('http', [], [], 80, 'path/subpath', [], 'fragment')).
test(uri108) :- uri_parse("http:/path/subpath", uri('http', [], [], 80, 'path/subpath', [], [])).
test(uri109) :- uri_parse("http:/path?query#fragment", uri('http', [], [], 80, 'path', 'query', 'fragment')).
test(uri110) :- uri_parse("http:/path?query", uri('http', [], [], 80, 'path', 'query', [])).
test(uri111) :- uri_parse("http:/path#fragment", uri('http', [], [], 80, 'path', [], 'fragment')).
test(uri112) :- uri_parse("http:/path", uri('http', [], [], 80, 'path', [], [])).
test(uri113) :- uri_parse("http:/?query#fragment", uri('http', [], [], 80, [], 'query', 'fragment')).
test(uri114) :- uri_parse("http:/#fragment", uri('http', [], [], 80, [], [], 'fragment')).
test(uri115) :- uri_parse("http:/?query", uri('http', [], [], 80, [], 'query', [])).
test(uri116) :- uri_parse("http:/", uri('http', [], [], 80, [], [], [])).
test(uri117) :- (uri_parse("http:", uri('http', [], [], 80, [], [], []))).

test(uri_1) :- \+(uri_parse("http://", _)).
test(uri_2) :- \+(uri_parse("http:///path/subpath?query#fragment", _)).
test(uri_3) :- \+(uri_parse("http:///path/subpath?query", _)).
test(uri_4) :- \+(uri_parse("http:///path/subpath#fragment", _)).
test(uri_5) :- \+(uri_parse("http:///path/subpath", _)).
test(uri_6) :- \+(uri_parse("http:///path?query#fragment", _)).
test(uri_7) :- \+(uri_parse("http:///path?query", _)).
test(uri_8) :- \+(uri_parse("http:///path#fragment", _)).
test(uri_9) :- \+(uri_parse("http:///path", _)).
test(uri_10) :- \+(uri_parse("http:///?query#fragment", _)).
test(uri_11) :- \+(uri_parse("http:///?query", _)).
test(uri_12) :- \+(uri_parse("http:///#fragment", _)).
test(uri_13) :- \+(uri_parse("http:///", _)).
test(uri_14) :- \+(uri_parse("http://", _)).
test(uri_15) :- \+(uri_parse("http://host?query#fragment", _)).
test(uri_16) :- \+(uri_parse("http://host?query", _)).
test(uri_17) :- \+(uri_parse("http://host#fragment", _)).
test(uri_18) :- \+(uri_parse("://host/path/subpath?query#fragment", _)).
test(uri_19) :- \+(uri_parse("://host/path/subpath?query", _)).
test(uri_20) :- \+(uri_parse("://host/path/subpath#fragment", _)).
test(uri_21) :- \+(uri_parse("://host/path/subpath", _)).
test(uri_22) :- \+(uri_parse("://host/path?query#fragment", _)).
test(uri_23) :- \+(uri_parse("://host/path?query", _)).
test(uri_24) :- \+(uri_parse("://host/path#fragment", _)).
test(uri_25) :- \+(uri_parse("://host/path", _)).
test(uri_26) :- \+(uri_parse("://host/?query#fragment", _)).
test(uri_27) :- \+(uri_parse("://host/?query", _)).
test(uri_28) :- \+(uri_parse("://host/#fragment", _)).
test(uri_29) :- \+(uri_parse("://host/", _)).
test(uri_30) :- \+(uri_parse("://host", _)).
test(uri_31) :- \+(uri_parse("http//host/path/subpath?query#fragment", _)).
test(uri_32) :- \+(uri_parse("http//host/path/subpath?query", _)).
test(uri_33) :- \+(uri_parse("http//host/path/subpath#fragment", _)).
test(uri_34) :- \+(uri_parse("http//host/path/subpath", _)).
test(uri_35) :- \+(uri_parse("http//host/path?query#fragment", _)).
test(uri_36) :- \+(uri_parse("http//host/path?query", _)).
test(uri_37) :- \+(uri_parse("http//host/path#fragment", _)).
test(uri_38) :- \+(uri_parse("http//host/path", _)).
test(uri_39) :- \+(uri_parse("http//host/?query#fragment", _)).
test(uri_40) :- \+(uri_parse("http//host/?query", _)).
test(uri_41) :- \+(uri_parse("http//host/#fragment", _)).
test(uri_42) :- \+(uri_parse("http//host/", _)).
test(uri_43) :- \+(uri_parse("http//host", _)).

test(uri_44) :- \+(uri_parse("http:path/subpath?query#fragment", _)).
test(uri_45) :- \+(uri_parse("http:path/subpath?query", _)).
test(uri_46) :- \+(uri_parse("http:path/subpath#fragment", _)).
test(uri_47) :- \+(uri_parse("http:path/subpath", _)).
test(uri_48) :- \+(uri_parse("http:path?query#fragment", _)).
test(uri_49) :- \+(uri_parse("http:path?query", _)).
test(uri_50) :- \+(uri_parse("http:path#fragment", _)).
test(uri_51) :- \+(uri_parse("http:path", _)).
test(uri_52) :- \+(uri_parse("http:?query#fragment", _)).
test(uri_53) :- \+(uri_parse("http:#fragment", _)).
test(uri_54) :- \+(uri_parse("http:?query", _)).

test(uri_55) :- \+(uri_parse("http/path/subpath?query#fragment", _)).
test(uri_56) :- \+(uri_parse("http/path/subpath?query", _)).
test(uri_57) :- \+(uri_parse("http/path/subpath#fragment", _)).
test(uri_58) :- \+(uri_parse("http/path/subpath", _)).
test(uri_59) :- \+(uri_parse("http/path?query#fragment", _)).
test(uri_60) :- \+(uri_parse("http/path?query", _)).
test(uri_61) :- \+(uri_parse("http/path#fragment", _)).
test(uri_62) :- \+(uri_parse("http/path", _)).
test(uri_63) :- \+(uri_parse("http/?query#fragment", _)).
test(uri_64) :- \+(uri_parse("http/#fragment", _)).
test(uri_65) :- \+(uri_parse("http/?query", _)).
test(uri_66) :- \+(uri_parse("http/", _)).

test(uri_67) :- \+(uri_parse("httppath/subpath?query#fragment", _)).
test(uri_68) :- \+(uri_parse("httppath/subpath?query", _)).
test(uri_69) :- \+(uri_parse("httppath/subpath#fragment", _)).
test(uri_70) :- \+(uri_parse("httppath/subpath", _)).
test(uri_71) :- \+(uri_parse("httppath?query#fragment", _)).
test(uri_72) :- \+(uri_parse("httppath?query", _)).
test(uri_73) :- \+(uri_parse("httppath#fragment", _)).
test(uri_74) :- \+(uri_parse("httppath", _)).
test(uri_75) :- \+(uri_parse("http?query#fragment", _)).
test(uri_76) :- \+(uri_parse("http#fragment", _)).
test(uri_77) :- \+(uri_parse("http?query", _)).
test(uri_78) :- \+(uri_parse("http", _)).

test(uri_79) :- \+(uri_parse(":/path/subpath?query#fragment", _)).
test(uri_80) :- \+(uri_parse(":/path/subpath?query", _)).
test(uri_81) :- \+(uri_parse(":/path/subpath#fragment", _)).
test(uri_82) :- \+(uri_parse(":/path/subpath", _)).
test(uri_83) :- \+(uri_parse(":/path?query#fragment", _)).
test(uri_84) :- \+(uri_parse(":/path?query", _)).
test(uri_85) :- \+(uri_parse(":/path#fragment", _)).
test(uri_86) :- \+(uri_parse(":/path", _)).
test(uri_87) :- \+(uri_parse(":/?query#fragment", _)).
test(uri_88) :- \+(uri_parse(":/#fragment", _)).
test(uri_89) :- \+(uri_parse(":/?query", _)).
test(uri_90) :- \+(uri_parse(":/", _)).

test(uri_91) :- \+(uri_parse(":path/subpath?query#fragment", _)).
test(uri_92) :- \+(uri_parse(":path/subpath?query", _)).
test(uri_93) :- \+(uri_parse(":path/subpath#fragment", _)).
test(uri_94) :- \+(uri_parse(":path/subpath", _)).
test(uri_95) :- \+(uri_parse(":path?query#fragment", _)).
test(uri_96) :- \+(uri_parse(":path?query", _)).
test(uri_97) :- \+(uri_parse(":path#fragment", _)).
test(uri_98) :- \+(uri_parse(":path", _)).
test(uri_99) :- \+(uri_parse(":?query#fragment", _)).
test(uri_100) :- \+(uri_parse(":#fragment", _)).
test(uri_101) :- \+(uri_parse(":?query", _)).
test(uri_102) :- \+(uri_parse(":", _)).

:- end_tests(uri_parse).

:- run_tests.