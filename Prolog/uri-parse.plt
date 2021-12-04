:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', [], 'google.com', [], [], [], [])).
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
test(userinfo1) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', [], [], [], [])).
test(userinfo2) :- uri_parse("http://user_info@host", uri('http', 'user_info', 'host', [], [], [], [])).
test(userinfo3) :- uri_parse("http://user123info@host", uri('http', 'user123info', 'host', [], [], [], [])).
test(userinfo_1) :- \+(uri_parse("http://user@info@host", _)).
test(userinfo_2) :- \+(uri_parse("http://userin:fo@host", _)).
test(userinfo_3) :- \+(uri_parse("http://userin/fo@host", _)).
test(userinfo_4) :- \+(uri_parse("http://userin?fo@host", _)).
test(userinfo_5) :- \+(uri_parse("http://userin#fo@host", _)).
test(userinfo_6) :- \+(uri_parse("http://userin fo@host", _)).
test(userinfo_7) :- \+(uri_parse("http://userinfo@", _)).
test(userinfo_8) :- \+(uri_parse("http://@", _)).

% TEST HOST
test(host1) :- uri_parse("scheme://host", uri(_, _, 'host', _, _, _, _)).
test(host2) :- uri_parse("scheme://userinfo@host", uri(_, 'userinfo', 'host', _, _, _, _)).
test(host3) :- uri_parse("scheme://host:123", uri(_, _, 'host', '123', _, _, _)).
test(host4) :- uri_parse("scheme://userinfo@host:123", uri(_, 'userinfo', 'host', '123', _, _, _)).
test(host5) :- uri_parse("scheme://host/path", uri(_, _, 'host', _, 'path', _, _)).
test(host6) :- uri_parse("scheme://userinfo@host.com:123", uri(_, 'userinfo', 'host.com', '123', _, _, _)).
test(host_1) :- \+(uri_parse("scheme://userinfo@ho?st:123", uri(_, _, _, _, _, _, _))).
test(host_2) :- \+(uri_parse("scheme://userinfo@ho@st:123", uri(_, _, _, _, _, _, _))).
test(host_3) :- \+(uri_parse("scheme://userinfo@ho:st:123", uri(_, _, _, _, _, _, _))).
test(host_4) :- \+(uri_parse("scheme://userinfo@ho/st:123", uri(_, _, _, _, _, _, _))).
test(host_5) :- \+(uri_parse("scheme://userinfo@ho#st:123", uri(_, _, _, _, _, _, _))).
test(host_6) :- \+(uri_parse("scheme://userinfo@ho st:123", uri(_, _, _, _, _, _, _))).

% TEST PORT
test(port1) :- uri_parse("scheme://host:123", uri(_, _, 'host', '123', _, _, _)).
test(port2) :- uri_parse("scheme://host:123/path", uri(_, _, 'host', '123', 'path', _, _)).
test(port3) :- uri_parse("http://host:123", uri(_, _, _, '123', _, _, _)).
test(port4) :- uri_parse("http://host", uri(_, _, _, '80', _, _, _)).
test(port5) :- uri_parse("https://host:123", uri(_, _, _, '123', _, _, _)).
test(port6) :- uri_parse("https://host", uri(_, _, _, '80', _, _, _)).
test(port_1) :- \+(uri_parse("scheme://host:1_23", uri(_, _, _, _, _, _, _))).
test(port_2) :- \+(uri_parse("scheme://host:1a23", uri(_, _, _, _, _, _, _))).
test(port_3) :- \+(uri_parse("scheme://host:", uri(_, _, _, _, _, _, _))).
test(port_4) :- \+(uri_parse("scheme://host: ", uri(_, _, _, _, _, _, _))).

% TEST PATH
test(path1) :- uri_parse("scheme://host/path", uri(_, _, _, _, 'path', _, _)).
test(path2) :- uri_parse("scheme://host/path/prova", uri(_, _, _, _, 'path/prova', _, _)).
test(path3) :- uri_parse("scheme://host/", uri(_, _, _, _, [], _, _)).
test(path4) :- uri_parse("scheme://host", uri(_, _, _, _, [], _, _)).
test(path5) :- uri_parse("scheme://host/path?query", uri(_, _, 'host', _, 'path', 'query', _)).
test(path6) :- uri_parse("scheme://host/pro va", uri(_, _, _, _, 'pro%20va', _, _)).
test(path7) :- uri_parse("scheme://host/pro.va", uri(_, _, _, _, 'pro.va', _, _)).
test(path_1) :- \+(uri_parse("scheme://host/path/", uri(_, _, _, _, _, _, _))).
test(path_2) :- \+(uri_parse("scheme://host/p:ath", uri(_, _, _, _, _, _, _))).
test(path_3) :- \+(uri_parse("scheme://host/p@ath", uri(_, _, _, _, _, _, _))).

% TEST QUERY
test(query1) :- uri_parse("scheme://host/?query", uri(_, _, _, _, _, 'query', _)).
test(query2) :- uri_parse("scheme://host/?qu ery", uri(_, _, _, _, _, 'qu%20ery', _)).
test(query3) :- uri_parse("scheme://host/?qu.ery", uri(_, _, _, _, _, 'qu.ery', _)).
test(query4) :- uri_parse("scheme://host/?qu:ery", uri(_, _, _, _, _, 'qu:ery', _)).
test(query5) :- uri_parse("scheme://host/?qu@ery", uri(_, _, _, _, _, 'qu@ery', _)).
test(query6) :- uri_parse("scheme://host/?qu/ery", uri(_, _, _, _, _, 'qu/ery', _)).
test(query7) :- uri_parse("scheme://host/?qu?ery", uri(_, _, _, _, _, 'qu?ery', _)).
test(query8) :- uri_parse("scheme://host", uri(_, _, _, _, _, [], _)).
test(query9) :- uri_parse("scheme://host/", uri(_, _, _, _, _, [], _)).
test(query10) :- uri_parse("scheme://host/path?query#fragment", uri(_, _, _, _, 'path', 'query', 'fragment')).
test(query_1) :- \+(uri_parse("scheme://host/?", uri(_, _, _, _, _, _, _))).
test(query_2) :- \+(uri_parse("scheme://host/?#", uri(_, _, _, _, _, _, _))).

% TEST FRAGMENT
test(fragment1) :- uri_parse("scheme://host/#frag", uri(_, _, _, _, _, _, 'frag')).
test(fragment2) :- uri_parse("scheme://host/path?query#frag", uri(_, _, _, _, 'path', 'query', 'frag')).
test(fragment3) :- uri_parse("scheme://host/?query#frag", uri(_, _, _, _, _, 'query', 'frag')).
test(fragment4) :- uri_parse("scheme://host", uri(_, _, _, _, _, _, [])).
test(fragment_1) :- \+(uri_parse("scheme://host/#", uri(_, _, _, _, _, _, _))).

:- end_tests(uri_parse).