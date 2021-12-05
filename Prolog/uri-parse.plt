:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', [], 'google.com', '80', [], [], [])).
test(schema2) :- uri_parse("h11ps://google.com", uri('h11ps', [], 'google.com', [], [], [], [])).
test(schema3) :- uri_parse("_http_://google.com", uri('_http_', [], 'google.com', [], [], [], [])).
test(schema_1) :- \+(uri_parse("htt p://google.com", _)).
test(schema_2) :- \+(uri_parse("htt:p://google.com", _)).

% TEST SCHEMA MAILTO
test(mailto1) :- uri_parse("mailto:userinfo", uri('mailto', 'userinfo', [], [], [], [], [])).
test(mailto2) :- uri_parse("mailto:userinfo@host", uri('mailto', 'userinfo', 'host', [], [], [], [])).
test(mailto_1) :- \+(uri_parse("mailto:", _)).
test(mailto_2) :- \+(uri_parse("mailto:userinfo@", _)).
test(mailto_3) :- \+(uri_parse("mailto:userinfo@host?query", _)).

% TEST SCHEMA FAX
test(fax1) :- uri_parse("fax:userinfo", uri('fax', 'userinfo', [], [], [], [], [])).
test(fax2) :- uri_parse("fax:user123info", uri('fax', 'user123info', [], [], [], [], [])).
test(fax_1) :- \+(uri_parse("fax:", _)).
test(fax_2) :- \+(uri_parse("fax:user info", _)).
test(fax_3) :- \+(uri_parse("fax:userinfo@host", _)).
test(fax_4) :- \+(uri_parse("fax:userinfo/path", _)).

% TEST SCHEMA TEL
test(tel1) :- uri_parse("tel:userinfo", uri('tel', 'userinfo', [], [], [], [], [])).
test(tel2) :- uri_parse("tel:user123info", uri('tel', 'user123info', [], [], [], [], [])).
test(tel3) :- uri_parse("tel:0293564242", uri('tel', '0293564242', [], [], [], [], [])).
test(tel_1) :- \+(uri_parse("tel:", _)).
test(tel_2) :- \+(uri_parse("tel:user info", _)).
test(tel_3) :- \+(uri_parse("tel:userinfo@host", _)).
test(tel_4) :- \+(uri_parse("tel:userinfo/path", _)).

% TEST SCHEMA NEWS
test(news1) :- uri_parse("news:host", uri('news', [], 'host', [], [], [], [])).
test(news2) :- uri_parse("news:host.subhost", uri('news', [], 'host.subhost', [], [], [], [])).
test(news2) :- uri_parse("news:ho123st", uri('news', [], 'ho123st', [], [], [], [])).
test(news_2) :- \+(uri_parse("news:ho st", _)).
test(news_3) :- \+(uri_parse("news:ho/st", _)).
test(news_4) :- \+(uri_parse("news:host/path", _)).
test(news_5) :- \+(uri_parse("news:host/path?query", _)).
test(news_6) :- \+(uri_parse("news:host/path?query#fragment", _)).
test(news_7) :- \+(uri_parse("news:host:80", _)).
test(news_8) :- \+(uri_parse("news:userinfo@host", _)).

% TEST SCHEMA ZOS
test(zos1) :- uri_parse("zos://host/id44(id8)", uri('zos', [], 'host', [], 'id44(id8)', [], [])).
test(zos2) :- uri_parse("zos://userinfo@host/id44(id8)", uri('zos', 'userinfo', 'host', [], 'id44(id8)', [], [])).
test(zos3) :- uri_parse("zos://userinfo@host:4832/id44(id8)", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', [], [])).
test(zos4) :- uri_parse("zos://userinfo@host:4832/id44(id8)?query", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', 'query', [])).
test(zos5) :- uri_parse("zos://userinfo@host:4832/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', 'query', 'fragment')).
test(zos6) :- uri_parse("zos://host/id.44(id8)", uri('zos', [], 'host', [], 'id.44(id8)', [], [])).
test(zos7) :- uri_parse("zos://host/i.d.4.4(id8)", uri('zos', [], 'host', [], 'i.d.4.4(id8)', [], [])).
test(zos8) :- uri_parse("zos://host/i.d.4.4", uri('zos', [], 'host', [], 'i.d.4.4', [], [])).
test(zos_1) :- \+(uri_parse("zos://host", _)).
test(zos_2) :- \+(uri_parse("zos://host/.i.d", _)).
test(zos_3) :- \+(uri_parse("zos://host/.", _)).
test(zos_4) :- \+(uri_parse("zos://host/", _)).
test(zos_5) :- \+(uri_parse("zos://host/012345678901234567890123456789012345678901234(id)", _)).
test(zos_6) :- \+(uri_parse("zos://host/path(012345678)", _)).
test(zos_7) :- \+(uri_parse("zos://host/012345678901234567890123456789012345678901234(012345678)", _)).
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

% TEST USERINFO
test(userinfo1) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', '80', [], [], [])).
test(userinfo2) :- uri_parse("http://user_info@host", uri('http', 'user_info', 'host', '80', [], [], [])).
test(userinfo3) :- uri_parse("http://user123info@host", uri('http', 'user123info', 'host', '80', [], [], [])).
test(userinfo_1) :- \+(uri_parse("http://user@info@host", _)).
test(userinfo_2) :- \+(uri_parse("http://userin:fo@host", _)).
test(userinfo_3) :- \+(uri_parse("http://userin/fo@host", _)).
test(userinfo_4) :- \+(uri_parse("http://userin?fo@host", _)).
test(userinfo_5) :- \+(uri_parse("http://userin#fo@host", _)).
test(userinfo_6) :- \+(uri_parse("http://userin fo@host", _)).
test(userinfo_7) :- \+(uri_parse("http://userinfo@", _)).
test(userinfo_8) :- \+(uri_parse("http://@", _)).

% TEST HOST
test(host1) :- uri_parse("scheme://host", uri('scheme', [], 'host', [], [], [], [])).
test(host2) :- uri_parse("scheme://userinfo@host", uri('scheme', 'userinfo', 'host', [], [], [], [])).
test(host3) :- uri_parse("scheme://host:123", uri('scheme', [], 'host', '123', [], [], [])).
test(host4) :- uri_parse("scheme://userinfo@host:123", uri('scheme', 'userinfo', 'host', '123', [], [], [])).
test(host5) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', [], 'path', [], [])).
test(host6) :- uri_parse("scheme://userinfo@host.com:123", uri('scheme', 'userinfo', 'host.com', '123', [], [], [])).
test(host_1) :- \+(uri_parse("scheme://userinfo@ho?st:123", _)).
test(host_2) :- \+(uri_parse("scheme://userinfo@ho@st:123", _)).
test(host_3) :- \+(uri_parse("scheme://userinfo@ho:st:123", _)).
test(host_4) :- \+(uri_parse("scheme://userinfo@ho/st:123", _)).
test(host_5) :- \+(uri_parse("scheme://userinfo@ho#st:123", _)).
test(host_6) :- \+(uri_parse("scheme://userinfo@ho st:123", _)).

% TEST PORT
test(port1) :- uri_parse("scheme://host:123", uri('scheme', [], 'host', '123', [], [], [])).
test(port2) :- uri_parse("scheme://host:123/path", uri('scheme', [], 'host', '123', 'path', [], [])).
test(port3) :- uri_parse("http://host:123", uri('http', [], 'host', '123', [], [], [])).
test(port4) :- uri_parse("http://host", uri('http', [], 'host', '80', [], [], [])).
test(port5) :- uri_parse("https://host:123", uri('https', [], 'host', '123', [], [], [])).
test(port6) :- uri_parse("https://host", uri('https', [], 'host', '80', [], [], [])).
test(port_1) :- \+(uri_parse("scheme://host:1_23", _)).
test(port_2) :- \+(uri_parse("scheme://host:1a23", _)).
test(port_3) :- \+(uri_parse("scheme://host:", _)).
test(port_4) :- \+(uri_parse("scheme://host: ", _)).

% TEST PATH
test(path1) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', [], 'path', [], [])).
test(path2) :- uri_parse("scheme://host/path/prova", uri('scheme', [], 'host', [], 'path/prova', [], [])).
test(path3) :- uri_parse("scheme://host/", uri('scheme', [], 'host', [], [], [], [])).
test(path4) :- uri_parse("scheme://host", uri('scheme', [], 'host', [], [], [], [])).
test(path5) :- uri_parse("scheme://host/path?query", uri('scheme', [], 'host', _, 'path', 'query', [])).
test(path6) :- uri_parse("scheme://host/pro va", uri('scheme', [], 'host', [], 'pro%20va', [], [])).
test(path7) :- uri_parse("scheme://host/pro.va", uri('scheme', [], 'host', [], 'pro.va', [], [])).
test(path_1) :- \+(uri_parse("scheme://host/path/", _)).
test(path_2) :- \+(uri_parse("scheme://host/p:ath", _)).
test(path_3) :- \+(uri_parse("scheme://host/p@ath", _)).

% TEST QUERY
test(query1) :- uri_parse("scheme://host/?query", uri('scheme', [], 'host', [], [], 'query', [])).
test(query2) :- uri_parse("scheme://host/?qu ery", uri('scheme', [], 'host', [], [], 'qu%20ery', [])).
test(query3) :- uri_parse("scheme://host/?qu.ery", uri('scheme', [], 'host', [], [], 'qu.ery', [])).
test(query4) :- uri_parse("scheme://host/?qu:ery", uri('scheme', [], 'host', [], [], 'qu:ery', [])).
test(query5) :- uri_parse("scheme://host/?qu@ery", uri('scheme', [], 'host', [], [], 'qu@ery', [])).
test(query6) :- uri_parse("scheme://host/?qu/ery", uri('scheme', [], 'host', [], [], 'qu/ery', [])).
test(query7) :- uri_parse("scheme://host/?qu?ery", uri('scheme', [], 'host', [], [], 'qu?ery', [])).
test(query8) :- uri_parse("scheme://host", uri('scheme', [], 'host', [], [], [], [])).
test(query9) :- uri_parse("scheme://host/", uri('scheme', [], 'host', [], [], [], [])).
test(query10) :- uri_parse("scheme://host/path?query#fragment", uri('scheme', [], 'host', [], 'path', 'query', 'fragment')).
test(query_1) :- \+(uri_parse("scheme://host/?", _)).
test(query_2) :- \+(uri_parse("scheme://host/?#", _)).

% TEST FRAGMENT
test(fragment1) :- uri_parse("scheme://host/#frag", uri('scheme', [], 'host', [], [], [], 'frag')).
test(fragment2) :- uri_parse("scheme://host/path?query#frag", uri('scheme', [], 'host', [], 'path', 'query', 'frag')).
test(fragment3) :- uri_parse("scheme://host/?query#frag", uri('scheme', [], 'host', [], [], 'query', 'frag')).
test(fragment4) :- uri_parse("scheme://host", uri('scheme', [], 'host', [], [], [], [])).
test(fragment_1) :- \+(uri_parse("scheme://host/#", _)).

:- end_tests(uri_parse).