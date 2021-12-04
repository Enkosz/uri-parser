:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', _, _, _, _, _, _)).
test(schema2) :- uri_parse("h11ps://google.com", uri('h11ps', _, _, _, _, _, _)).
test(schema3) :- uri_parse("_http_://google.com", uri('_http_', _, _, _, _, _, _)).
test(schema_1) :- \+(uri_parse("htt p://google.com", uri(_, _, _, _, _, _, _))).
test(schema_2) :- \+(uri_parse("htt:p://google.com", uri(_, _, _, _, _, _, _))).

% TEST SCHEMA MAILTO
test(mailto1) :- uri_parse("mailto:userinfo", uri('mailto', 'userinfo', _, _, _, _, _)).
test(mailto2) :- uri_parse("mailto:userinfo@host", uri('mailto', 'userinfo', 'host', _, _, _, _)).
test(mailto_1) :- \+(uri_parse("mailto:", uri(_, _, _, _, _, _, _))).
test(mailto_2) :- \+(uri_parse("mailto:userinfo@", uri(_, _, _, _, _, _, _))).

% TEST SCHEMA NEWS
test(news1) :- uri_parse("news:host", uri('news', _, 'host', _, _, _, _)).
test(news2) :- uri_parse("news:host.subhost", uri('news', _, 'host.subhost', _, _, _, _)).
test(news2) :- uri_parse("news:ho123st", uri('news', _, 'ho123st', _, _, _, _)).
test(news_1) :- \+(uri_parse("invalidschema:host", uri(_, _, _, _, _, _, _))).
test(news_2) :- \+(uri_parse("news:ho st", uri(_, _, _, _, _, _, _))).
test(news_3) :- \+(uri_parse("news:ho/st", uri(_, _, _, _, _, _, _))).
test(news_4) :- \+(uri_parse("news:host/path", uri(_, _, _, _, _, _, _))).
test(news_5) :- \+(uri_parse("news:host/path?query", uri(_, _, _, _, _, _, _))).
test(news_6) :- \+(uri_parse("news:host/path?query#fragment", uri(_, _, _, _, _, _, _))).
test(news_7) :- \+(uri_parse("news:host:80", uri(_, _, _, _, _, _, _))).
test(news_8) :- \+(uri_parse("news:userinfo@host", uri(_, _, _, _, _, _, _))).

% TEST USERINFO
test(userinfo1) :- uri_parse("http://userinfo@host", uri(_, 'userinfo', 'host', _, _, _, _)).
test(userinfo2) :- uri_parse("http://user_info@host", uri(_, 'user_info', 'host', _, _, _, _)).
test(userinfo3) :- uri_parse("http://user123info@host", uri(_, 'user123info', 'host', _, _, _, _)).
test(userinfo_1) :- \+(uri_parse("http://user@info@host", uri(_, _, _, _, _, _, _))).
test(userinfo_2) :- \+(uri_parse("http://userin:fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo_3) :- \+(uri_parse("http://userin/fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo_4) :- \+(uri_parse("http://userin?fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo_5) :- \+(uri_parse("http://userin#fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo_6) :- \+(uri_parse("http://userin fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo_7) :- \+(uri_parse("http://userinfo@", uri(_, _, _, _, _, _, _))).
test(userinfo_8) :- \+(uri_parse("http://@", uri(_, _, _, _, _, _, _))).

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

% TEST INTEGRATION (Discuterne)

:- end_tests(uri_parse).