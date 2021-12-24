 :- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', [], 'google.com', 80, [], [], [])).
test(schema2) :- uri_parse("h11ps://google.com", uri('h11ps', [], 'google.com', 80, [], [], [])).
test(schema3) :- uri_parse("_http_://google.com", uri('_http_', [], 'google.com', 80, [], [], [])).
test(schema_1) :- \+(uri_parse("://google.com", _)).
test(schema_2) :- \+(uri_parse("@://google.com", _)).
test(schema_3) :- \+(uri_parse("h:h://google.com", _)).
test(schema_4) :- \+(uri_parse("/hh://google.com", _)).
test(schema_5) :- \+(uri_parse("h#h://google.com", _)).
test(schema_6) :- \+(uri_parse("h?h://google.com", _)).
test(schema_7) :- \+(uri_parse(":://google.com", _)).

% TEST USERINFO
test(userinfo1) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', 80, [], [], [])).
test(userinfo2) :- uri_parse("http://user_info@host", uri('http', 'user_info', 'host', 80, [], [], [])).
test(userinfo3) :- uri_parse("http://user123info@host", uri('http', 'user123info', 'host', 80, [], [], [])).
test(userinfo4) :- uri_parse("http://user.@host", uri('http', 'user.', 'host', 80, [], [], []))
test(userinfo_1) :- \+(uri_parse("http://user@info@host", _)).
test(userinfo_2) :- \+(uri_parse("http://userin:fo@host", _)).
test(userinfo_3) :- \+(uri_parse("http://userin/fo@host", _)).
test(userinfo_4) :- \+(uri_parse("http://userin?fo@host", _)).
test(userinfo_5) :- \+(uri_parse("http://userin#fo@host", _)).
test(userinfo_6) :- \+(uri_parse("http://userin fo@host", _)).
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
test(host_1) :- \+(uri_parse("scheme://userinfo@ho?st:123", _)).
test(host_2) :- \+(uri_parse("scheme://userinfo@ho@st:123", _)).
test(host_3) :- \+(uri_parse("scheme://userinfo@ho:st:123", _)).
test(host_4) :- \+(uri_parse("scheme://userinfo@ho/st:123", _)).
test(host_5) :- \+(uri_parse("scheme://userinfo@ho#st:123", _)).
test(host_6) :- \+(uri_parse("scheme://userinfo@ho st:123", _)).
test(host_7) :- \+(uri_parse("scheme://host..com", _)).
test(host_8) :- \+(uri_parse("scheme://host.", _)).
test(host_9) :- \+(uri_parse("scheme://host:", _)).
test(host_10) :- \+(uri_parse("scheme://host@", _)).
test(host_11) :- \+(uri_parse("scheme://host?", _)).
test(host_12) :- \+(uri_parse("scheme://host#", _)).
test(host_13) :- \+(uri_parse("scheme://host ", _)).
test(host_14) :- \+(uri_parse("scheme://.host", _)).
test(host_15) :- \+(uri_parse("scheme:///host", _)).
test(host_16) :- \+(uri_parse("scheme://@host", _)).
test(host_17) :- \+(uri_parse("scheme://:host", _)).
test(host_18) :- \+(uri_parse("scheme://?host", _)).
test(host_19) :- \+(uri_parse("scheme://#host", _)).
test(host_20) :- \+(uri_parse("scheme:// host", _)).

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

% TEST PATH
test(path1) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', 80, 'path', [], [])).
test(path2) :- uri_parse("scheme://host/path/prova", uri('scheme', [], 'host', 80, 'path/prova', [], [])).
test(path3) :- uri_parse("scheme://host/", uri('scheme', [], 'host', 80, [], [], [])).
test(path4) :- uri_parse("scheme://host", uri('scheme', [], 'host', 80, [], [], [])).
test(path5) :- uri_parse("scheme://host/path?query", uri('scheme', [], 'host', 80, 'path', 'query', [])).
test(path6) :- uri_parse("scheme://host/pro va", uri('scheme', [], 'host', 80, 'pro%20va', [], [])).
test(path7) :- uri_parse("scheme://host/pro.va", uri('scheme', [], 'host', 80, 'pro.va', [], [])).
test(path8) :- uri_parse("scheme://host/path#fragment", uri('scheme', [], 'host', 80, 'path', [], 'fragment')).
test(path_1) :- \+(uri_parse("scheme://host/path/", _)).
test(path_2) :- \+(uri_parse("scheme://host/p:ath", _)).
test(path_3) :- \+(uri_parse("scheme://host/p@ath", _)).
test(path_4) :- \+(uri_parse("scheme://host//path", _)).
test(path_5) :- \+(uri_parse("scheme://host/@path", _)).
test(path_6) :- \+(uri_parse("scheme://host/:path", _)).
test(path_7) :- \+(uri_parse("scheme://host/path@", _)).
test(path_8) :- \+(uri_parse("scheme://host/path:", _)).
test(path_9) :- \+(uri_parse("scheme://host/path//com", _)).

% TEST QUERY
test(query1) :- uri_parse("scheme://host/?query", uri('scheme', [], 'host', 80, [], 'query', [])).
test(query2) :- uri_parse("scheme://host/?qu ery", uri('scheme', [], 'host', 80, [], 'qu%20ery', [])).
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
test(fragment5) :- uri_parse("scheme://host/#fr ag", uri('scheme', [], 'host', 80, [], [], 'fr%20ag')).
test(fragment_1) :- \+(uri_parse("scheme://host/#", _)).
test(fragment_2) :- \+(uri_parse("scheme://host#", _)).

% TEST SCHEMA MAILTO
test(mailto1) :- uri_parse("mailto:userinfo", uri('mailto', 'userinfo', [], [], [], [], [])).
test(mailto2) :- uri_parse("mailto:userinfo@host", uri('mailto', 'userinfo', 'host', [], [], [], [])).
test(mailto3) :- uri_parse("mailto:", uri('mailto', [], [], [], [], [], [])).
test(mailto_1) :- \+(uri_parse("mailto:userinfo@", _)).
test(mailto_2) :- \+(uri_parse("mailto:userinfo@host?query", _)).
test(mailto_3) :- \+(uri_parse("mailto:userinfo@host/path", _)).

% TEST SCHEMA FAX
test(fax1) :- uri_parse("fax:userinfo", uri('fax', 'userinfo', [], [], [], [], [])).
test(fax2) :- uri_parse("fax:user123info", uri('fax', 'user123info', [], [], [], [], [])).
test(fax3) :- uri_parse("fax:", uri('fax', [], [], [], [], [], [])).
test(fax_2) :- \+(uri_parse("fax:user info", _)).
test(fax_3) :- \+(uri_parse("fax:userinfo@host", _)).
test(fax_4) :- \+(uri_parse("fax:userinfo/path", _)).

% TEST SCHEMA TEL
test(tel1) :- uri_parse("tel:userinfo", uri('tel', 'userinfo', [], [], [], [], [])).
test(tel2) :- uri_parse("tel:user123info", uri('tel', 'user123info', [], [], [], [], [])).
test(tel3) :- uri_parse("tel:0293564242", uri('tel', '0293564242', [], [], [], [], [])).
test(tel4) :- uri_parse("tel:", uri('tel', [], [], [], [], [], [])).
test(tel_1) :- \+(uri_parse("tel:user info", _)).
test(tel_2) :- \+(uri_parse("tel:userinfo@host", _)).
test(tel_3) :- \+(uri_parse("tel:userinfo/path", _)).

% TEST SCHEMA NEWS
test(news1) :- uri_parse("news:host", uri('news', [], 'host', [], [], [], [])).
test(news2) :- uri_parse("news:host.subhost", uri('news', [], 'host.subhost', [], [], [], [])).
test(news3) :- uri_parse("news:ho123st", uri('news', [], 'ho123st', [], [], [], [])).
test(news4) :- uri_parse("news:", uri('news', [], [], [], [], [], [])).
test(news_1) :- \+(uri_parse("news:ho st", _)).
test(news_2) :- \+(uri_parse("news:ho/st", _)).
test(news_3) :- \+(uri_parse("news:host/path", _)).
test(news_4) :- \+(uri_parse("news:host/path?query", _)).
test(news_5) :- \+(uri_parse("news:host/path?query#fragment", _)).
test(news_6) :- \+(uri_parse("news:host:80", _)).
test(news_7) :- \+(uri_parse("news:userinfo@host", _)).

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
test(zos11) :- uri_parse("zos://host/", _).
test(zos12) :- uri_parse("zos://host", _).
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




% TEST INTEGRATION ---- URI
test(URI1) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', 'query', 'fragment')).
test(URI2) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', 'query', [])).
test(URI3) :- uri_parse("http://userinfo@host.subhost:123/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', [], 'fragment')).
test(URI4) :- uri_parse("http://userinfo@host.subhost:123/path/subpath", uri('http', 'userinfo', 'host.subhost', 123, 'path/subpath', [], [])).
test(URI5) :- uri_parse("http://userinfo@host.subhost:123/path?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path', 'query', 'fragment')).
test(URI6) :- uri_parse("http://userinfo@host.subhost:123/path?query", uri('http', 'userinfo', 'host.subhost', 123, 'path', 'query', [])).
test(URI7) :- uri_parse("http://userinfo@host.subhost:123/path#fragment", uri('http', 'userinfo', 'host.subhost', 123, 'path', [], 'fragment')).
test(URI8) :- uri_parse("http://userinfo@host.subhost:123/path", uri('http', 'userinfo', 'host.subhost', 123, 'path', [], [])).
test(URI9) :- uri_parse("http://userinfo@host.subhost:123/?query#fragment", uri('http', 'userinfo', 'host.subhost', 123, [], 'query', 'fragment')).
test(URI10) :- uri_parse("http://userinfo@host.subhost:123/?query", uri('http', 'userinfo', 'host.subhost', 123, [], 'query', [])).
test(URI11) :- uri_parse("http://userinfo@host.subhost:123/#fragment", uri('http', 'userinfo', 'host.subhost', 123, [], [], 'fragment')).
test(URI12) :- uri_parse("http://userinfo@host.subhost:123/", uri('http', 'userinfo', 'host.subhost', 123, [], [], [])).
test(URI13) :- uri_parse("http://userinfo@host.subhost:123", uri('http', 'userinfo', 'host.subhost', 123, [], [], [])).

test(URI14) :- uri_parse("http://userinfo@host.subhost/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', 'query', 'fragment')).
test(URI15) :- uri_parse("http://userinfo@host.subhost/path/subpath?query", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', 'query', [])).
test(URI16) :- uri_parse("http://userinfo@host.subhost/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', [], 'fragment')).
test(URI17) :- uri_parse("http://userinfo@host.subhost/path/subpath", uri('http', 'userinfo', 'host.subhost', 80, 'path/subpath', [], [])).
test(URI18) :- uri_parse("http://userinfo@host.subhost/path?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path', 'query', 'fragment')).
test(URI19) :- uri_parse("http://userinfo@host.subhost/path?query", uri('http', 'userinfo', 'host.subhost', 80, 'path', 'query', [])).
test(URI20) :- uri_parse("http://userinfo@host.subhost/path#fragment", uri('http', 'userinfo', 'host.subhost', 80, 'path', [], 'fragment')).
test(URI21) :- uri_parse("http://userinfo@host.subhost/path", uri('http', 'userinfo', 'host.subhost', 80, 'path', [], [])).
test(URI22) :- uri_parse("http://userinfo@host.subhost/?query#fragment", uri('http', 'userinfo', 'host.subhost', 80, [], 'query', 'fragment')).
test(URI23) :- uri_parse("http://userinfo@host.subhost/?query", uri('http', 'userinfo', 'host.subhost', 80, [], 'query', [])).
test(URI24) :- uri_parse("http://userinfo@host.subhost/#fragment", uri('http', 'userinfo', 'host.subhost', 80, [], [], 'fragment')).
test(URI25) :- uri_parse("http://userinfo@host.subhost/", uri('http', 'userinfo', 'host.subhost', 80, [], [], [])).
test(URI26) :- uri_parse("http://userinfo@host.subhost", uri('http', 'userinfo', 'host.subhost', 80, [], [], [])).

test(URI27) :- uri_parse("http://host.subhost:123/path/subpath?query#fragment", uri('http', [], 'host.subhost', 123, 'path/subpath', 'query', 'fragment')).
test(URI28) :- uri_parse("http://host.subhost:123/path/subpath?query", uri('http', [], 'host.subhost', 123, 'path/subpath', 'query', [])).
test(URI29) :- uri_parse("http://host.subhost:123/path/subpath#fragment", uri('http', [], 'host.subhost', 123, 'path/subpath', [], 'fragment')).
test(URI30) :- uri_parse("http://host.subhost:123/path/subpath", uri('http', [], 'host.subhost', 123, 'path/subpath', [], [])).
test(URI31) :- uri_parse("http://host.subhost:123/path?query#fragment", uri('http', [], 'host.subhost', 123, 'path', 'query', 'fragment')).
test(URI32) :- uri_parse("http://host.subhost:123/path?query", uri('http', [], 'host.subhost', 123, 'path', 'query', [])).
test(URI33) :- uri_parse("http://host.subhost:123/path#fragment", uri('http', [], 'host.subhost', 123, 'path', [], 'fragment')).
test(URI34) :- uri_parse("http://host.subhost:123/path", uri('http', [], 'host.subhost', 123, 'path', [], [])).
test(URI35) :- uri_parse("http://host.subhost:123/?query#fragment", uri('http', [], 'host.subhost', 123, [], 'query', 'fragment')).
test(URI36) :- uri_parse("http://host.subhost:123/?query", uri('http', [], 'host.subhost', 123, [], 'query', [])).
test(URI37) :- uri_parse("http://host.subhost:123/#fragment", uri('http', [], 'host.subhost', 123, [], [], 'fragment')).
test(URI38) :- uri_parse("http://host.subhost:123/", uri('http', [], 'host.subhost', 123, [], [], [])).
test(URI39) :- uri_parse("http://host.subhost:123", uri('http', [], 'host.subhost', 123, [], [], [])).

test(URI40) :- uri_parse("http://host.subhost/path/subpath?query#fragment", uri('http', [], 'host.subhost', 80, 'path/subpath', 'query', 'fragment')).
test(URI41) :- uri_parse("http://host.subhost/path/subpath?query", uri('http', [], 'host.subhost', 80, 'path/subpath', 'query', [])).
test(URI42) :- uri_parse("http://host.subhost/path/subpath#fragment", uri('http', [], 'host.subhost', 80, 'path/subpath', [], 'fragment')).
test(URI43) :- uri_parse("http://host.subhost/path/subpath", uri('http', [], 'host.subhost', 80, 'path/subpath', [], [])).
test(URI44) :- uri_parse("http://host.subhost/path?query#fragment", uri('http', [], 'host.subhost', 80, 'path', 'query', 'fragment')).
test(URI45) :- uri_parse("http://host.subhost/path?query", uri('http', [], 'host.subhost', 80, 'path', 'query', [])).
test(URI46) :- uri_parse("http://host.subhost/path#fragment", uri('http', [], 'host.subhost', 80, 'path', [], 'fragment')).
test(URI47) :- uri_parse("http://host.subhost/path", uri('http', [], 'host.subhost', 80, 'path', [], [])).
test(URI48) :- uri_parse("http://host.subhost/?query#fragment", uri('http', [], 'host.subhost', 80, [], 'query', 'fragment')).
test(URI49) :- uri_parse("http://host.subhost/?query", uri('http', [], 'host.subhost', 80, [], 'query', [])).
test(URI50) :- uri_parse("http://host.subhost/#fragment", uri('http', [], 'host.subhost', 80, [], [], 'fragment')).
test(URI51) :- uri_parse("http://host.subhost/", uri('http', [], 'host.subhost', 80, [], [], [])).
test(URI52) :- uri_parse("http://host.subhost", uri('http', [], 'host.subhost', 80, [], [], [])).

test(URI53) :- uri_parse("http://userinfo@host:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host', 123, 'path/subpath', 'query', 'fragment')).
test(URI54) :- uri_parse("http://userinfo@host:123/path/subpath?query", uri('http', 'userinfo', 'host', 123, 'path/subpath', 'query', [])).
test(URI55) :- uri_parse("http://userinfo@host:123/path/subpath#fragment", uri('http', 'userinfo', 'host', 123, 'path/subpath', [], 'fragment')).
test(URI56) :- uri_parse("http://userinfo@host:123/path/subpath", uri('http', 'userinfo', 'host', 123, 'path/subpath', [], [])).
test(URI57) :- uri_parse("http://userinfo@host:123/path?query#fragment", uri('http', 'userinfo', 'host', 123, 'path', 'query', 'fragment')).
test(URI58) :- uri_parse("http://userinfo@host:123/path?query", uri('http', 'userinfo', 'host', 123, 'path', 'query', [])).
test(URI59) :- uri_parse("http://userinfo@host:123/path#fragment", uri('http', 'userinfo', 'host', 123, 'path', [], 'fragment')).
test(URI60) :- uri_parse("http://userinfo@host:123/path", uri('http', 'userinfo', 'host', 123, 'path', [], [])).
test(URI61) :- uri_parse("http://userinfo@host:123/?query#fragment", uri('http', 'userinfo', 'host', 123, [], 'query', 'fragment')).
test(URI62) :- uri_parse("http://userinfo@host:123/?query", uri('http', 'userinfo', 'host', 123, [], 'query', [])).
test(URI63) :- uri_parse("http://userinfo@host:123/#fragment", uri('http', 'userinfo', 'host', 123, [], [], 'fragment')).
test(URI64) :- uri_parse("http://userinfo@host:123/", uri('http', 'userinfo', 'host', 123, [], [], [])).
test(URI65) :- uri_parse("http://userinfo@host:123", uri('http', 'userinfo', 'host', 123, [], [], [])).

test(URI66) :- uri_parse("http://userinfo@host/path/subpath?query#fragment", uri('http', 'userinfo', 'host', 80, 'path/subpath', 'query', 'fragment')).
test(URI67) :- uri_parse("http://userinfo@host/path/subpath?query", uri('http', 'userinfo', 'host', 80, 'path/subpath', 'query', [])).
test(URI68) :- uri_parse("http://userinfo@host/path/subpath#fragment", uri('http', 'userinfo', 'host', 80, 'path/subpath', [], 'fragment')).
test(URI69) :- uri_parse("http://userinfo@host/path/subpath", uri('http', 'userinfo', 'host', 80, 'path/subpath', [], [])).
test(URI70) :- uri_parse("http://userinfo@host/path?query#fragment", uri('http', 'userinfo', 'host', 80, 'path', 'query', 'fragment')).
test(URI71) :- uri_parse("http://userinfo@host/path?query", uri('http', 'userinfo', 'host', 80, 'path', 'query', [])).
test(URI72) :- uri_parse("http://userinfo@host/path#fragment", uri('http', 'userinfo', 'host', 80, 'path', [], 'fragment')).
test(URI73) :- uri_parse("http://userinfo@host/path", uri('http', 'userinfo', 'host', 80, 'path', [], [])).
test(URI74) :- uri_parse("http://userinfo@host/?query#fragment", uri('http', 'userinfo', 'host', 80, [], 'query', 'fragment')).
test(URI75) :- uri_parse("http://userinfo@host/?query", uri('http', 'userinfo', 'host', 80, [], 'query', [])).
test(URI76) :- uri_parse("http://userinfo@host/#fragment", uri('http', 'userinfo', 'host', 80, [], [], 'fragment')).
test(URI77) :- uri_parse("http://userinfo@host/", uri('http', 'userinfo', 'host', 80, [], [], [])).
test(URI78) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', 80, [], [], [])).

test(URI79) :- uri_parse("http://host:123/path/subpath?query#fragment", uri('http', [], 'host', 123, 'path/subpath', 'query', 'fragment')).
test(URI80) :- uri_parse("http://host:123/path/subpath?query", uri('http', [], 'host', 123, 'path/subpath', 'query', [])).
test(URI81) :- uri_parse("http://host:123/path/subpath#fragment", uri('http', [], 'host', 123, 'path/subpath', [], 'fragment')).
test(URI82) :- uri_parse("http://host:123/path/subpath", uri('http', [], 'host', 123, 'path/subpath', [], [])).
test(URI83) :- uri_parse("http://host:123/path?query#fragment", uri('http', [], 'host', 123, 'path', 'query', 'fragment')).
test(URI84) :- uri_parse("http://host:123/path?query", uri('http', [], 'host', 123, 'path', 'query', [])).
test(URI85) :- uri_parse("http://host:123/path#fragment", uri('http', [], 'host', 123, 'path', [], 'fragment')).
test(URI86) :- uri_parse("http://host:123/path", uri('http', [], 'host', 123, 'path', [], [])).
test(URI87) :- uri_parse("http://host:123/?query#fragment", uri('http', [], 'host', 123, [], 'query', 'fragment')).
test(URI88) :- uri_parse("http://host:123/?query", uri('http', [], 'host', 123, [], 'query', [])).
test(URI89) :- uri_parse("http://host:123/#fragment", uri('http', [], 'host', 123, [], [], 'fragment')).
test(URI90) :- uri_parse("http://host:123/", uri('http', [], 'host', 123, [], [], [])).
test(URI91) :- uri_parse("http://host:123", uri('http', [], 'host', 123, [], [], [])).

test(URI92) :- uri_parse("http://host/path/subpath?query#fragment", uri('http', [], 'host', 80, 'path/subpath', 'query', 'fragment')).
test(URI93) :- uri_parse("http://host/path/subpath?query", uri('http', [], 'host', 80, 'path/subpath', 'query', [])).
test(URI94) :- uri_parse("http://host/path/subpath#fragment", uri('http', [], 'host', 80, 'path/subpath', [], 'fragment')).
test(URI95) :- uri_parse("http://host/path/subpath", uri('http', [], 'host', 80, 'path/subpath', [], [])).
test(URI96) :- uri_parse("http://host/path?query#fragment", uri('http', [], 'host', 80, 'path', 'query', 'fragment')).
test(URI97) :- uri_parse("http://host/path?query", uri('http', [], 'host', 80, 'path', 'query', [])).
test(URI98) :- uri_parse("http://host/path#fragment", uri('http', [], 'host', 80, 'path', [], 'fragment')).
test(URI99) :- uri_parse("http://host/path", uri('http', [], 'host', 80, 'path', [], [])).
test(URI100) :- uri_parse("http://host/?query#fragment", uri('http', [], 'host', 80, [], 'query', 'fragment')).
test(URI101) :- uri_parse("http://host/?query", uri('http', [], 'host', 80, [], 'query', [])).
test(URI102) :- uri_parse("http://host/#fragment", uri('http', [], 'host', 80, [], [], 'fragment')).
test(URI103) :- uri_parse("http://host/", uri('http', [], 'host', 80, [], [], [])).
test(URI104) :- uri_parse("http://host", uri('http', [], 'host', 80, [], [], [])).

test(URI105) :- uri_parse("http:/path/subpath?query#fragment", uri('http', [], [], 80, 'path/subpath', 'query', 'fragment')).
test(URI106) :- uri_parse("http:/path/subpath?query", uri('http', [], [], 80, 'path/subpath', 'query', [])).
test(URI107) :- uri_parse("http:/path/subpath#fragment", uri('http', [], [], 80, 'path/subpath', [], 'fragment')).
test(URI108) :- uri_parse("http:/path/subpath", uri('http', [], [], 80, 'path/subpath', [], [])).
test(URI109) :- uri_parse("http:/path?query#fragment", uri('http', [], [], 80, 'path', 'query', 'fragment')).
test(URI110) :- uri_parse("http:/path?query", uri('http', [], [], 80, 'path', 'query', [])).
test(URI111) :- uri_parse("http:/path#fragment", uri('http', [], [], 80, 'path', [], 'fragment')).
test(URI112) :- uri_parse("http:/path", uri('http', [], [], 80, 'path', [], [])).
test(URI113) :- uri_parse("http:/?query#fragment", uri('http', [], [], 80, [], 'query', 'fragment')).
test(URI114) :- uri_parse("http:/#fragment", uri('http', [], [], 80, [], [], 'fragment')).
test(URI115) :- uri_parse("http:/?query", uri('http', [], [], 80, [], 'query', [])).
test(URI116) :- uri_parse("http:/", uri('http', [], [], 80, [], [], [])).
test(URI117) :- (uri_parse("http:", uri()).

test(URI_1) :- \+(uri_parse("http://", _)).
test(URI_2) :- \+(uri_parse("http:///path/subpath?query#fragment", _)).
test(URI_3) :- \+(uri_parse("http:///path/subpath?query", _)).
test(URI_4) :- \+(uri_parse("http:///path/subpath#fragment", _)).
test(URI_5) :- \+(uri_parse("http:///path/subpath", _)).
test(URI_6) :- \+(uri_parse("http:///path?query#fragment", _)).
test(URI_7) :- \+(uri_parse("http:///path?query", _)).
test(URI_8) :- \+(uri_parse("http:///path#fragment", _)).
test(URI_9) :- \+(uri_parse("http:///path", _)).
test(URI_10) :- \+(uri_parse("http:///?query#fragment", _)).
test(URI_11) :- \+(uri_parse("http:///?query", _)).
test(URI_12) :- \+(uri_parse("http:///#fragment", _)).
test(URI_13) :- \+(uri_parse("http:///", _)).
test(URI_14) :- \+(uri_parse("http://", _)).
test(URI_15) :- \+(uri_parse("http://host?query#fragment", _)).
test(URI_16) :- \+(uri_parse("http://host?query", _)).
test(URI_17) :- \+(uri_parse("http://host#fragment", _)).
test(URI_18) :- \+(uri_parse("://host/path/subpath?query#fragment", _)).
test(URI_19) :- \+(uri_parse("://host/path/subpath?query", _)).
test(URI_20) :- \+(uri_parse("://host/path/subpath#fragment", _)).
test(URI_21) :- \+(uri_parse("://host/path/subpath", _)).
test(URI_22) :- \+(uri_parse("://host/path?query#fragment", _)).
test(URI_23) :- \+(uri_parse("://host/path?query", _)).
test(URI_24) :- \+(uri_parse("://host/path#fragment", _)).
test(URI_25) :- \+(uri_parse("://host/path", _)).
test(URI_26) :- \+(uri_parse("://host/?query#fragment", _)).
test(URI_27) :- \+(uri_parse("://host/?query", _)).
test(URI_28) :- \+(uri_parse("://host/#fragment", _)).
test(URI_29) :- \+(uri_parse("://host/", _)).
test(URI_30) :- \+(uri_parse("://host", _)).
test(URI_31) :- \+(uri_parse("http//host/path/subpath?query#fragment", _)).
test(URI_32) :- \+(uri_parse("http//host/path/subpath?query", _)).
test(URI_33) :- \+(uri_parse("http//host/path/subpath#fragment", _)).
test(URI_34) :- \+(uri_parse("http//host/path/subpath", _)).
test(URI_35) :- \+(uri_parse("http//host/path?query#fragment", _)).
test(URI_36) :- \+(uri_parse("http//host/path?query", _)).
test(URI_37) :- \+(uri_parse("http//host/path#fragment", _)).
test(URI_38) :- \+(uri_parse("http//host/path", _)).
test(URI_39) :- \+(uri_parse("http//host/?query#fragment", _)).
test(URI_40) :- \+(uri_parse("http//host/?query", _)).
test(URI_41) :- \+(uri_parse("http//host/#fragment", _)).
test(URI_42) :- \+(uri_parse("http//host/", _)).
test(URI_43) :- \+(uri_parse("http//host", _)).

test(URI_44) :- \+(uri_parse("http:path/subpath?query#fragment", _)).
test(URI_45) :- \+(uri_parse("http:path/subpath?query", _)).
test(URI_46) :- \+(uri_parse("http:path/subpath#fragment", _)).
test(URI_47) :- \+(uri_parse("http:path/subpath", _)).
test(URI_48) :- \+(uri_parse("http:path?query#fragment", _)).
test(URI_49) :- \+(uri_parse("http:path?query", _)).
test(URI_50) :- \+(uri_parse("http:path#fragment", _)).
test(URI_51) :- \+(uri_parse("http:path", _)).
test(URI_52) :- \+(uri_parse("http:?query#fragment", _)).
test(URI_53) :- \+(uri_parse("http:#fragment", _)).
test(URI_54) :- \+(uri_parse("http:?query", _)).

test(URI_55) :- \+(uri_parse("http/path/subpath?query#fragment", _)).
test(URI_57) :- \+(uri_parse("http/path/subpath?query", _)).
test(URI_58) :- \+(uri_parse("http/path/subpath#fragment", _)).
test(URI_59) :- \+(uri_parse("http/path/subpath", _)).
test(URI_60) :- \+(uri_parse("http/path?query#fragment", _)).
test(URI_61) :- \+(uri_parse("http/path?query", _)).
test(URI_62) :- \+(uri_parse("http/path#fragment", _)).
test(URI_63) :- \+(uri_parse("http/path", _)).
test(URI_64) :- \+(uri_parse("http/?query#fragment", _)).
test(URI_65) :- \+(uri_parse("http/#fragment", _)).
test(URI_66) :- \+(uri_parse("http/?query", _)).
test(URI_67) :- \+(uri_parse("http/", _)).

test(URI_68) :- \+(uri_parse("httppath/subpath?query#fragment", _)).
test(URI_69) :- \+(uri_parse("httppath/subpath?query", _)).
test(URI_70) :- \+(uri_parse("httppath/subpath#fragment", _)).
test(URI_71) :- \+(uri_parse("httppath/subpath", _)).
test(URI_72) :- \+(uri_parse("httppath?query#fragment", _)).
test(URI_73) :- \+(uri_parse("httppath?query", _)).
test(URI_74) :- \+(uri_parse("httppath#fragment", _)).
test(URI_75) :- \+(uri_parse("httppath", _)).
test(URI_76) :- \+(uri_parse("http?query#fragment", _)).
test(URI_77) :- \+(uri_parse("http#fragment", _)).
test(URI_78) :- \+(uri_parse("http?query", _)).
test(URI_79) :- \+(uri_parse("http", _)).

test(URI_80) :- \+(uri_parse(":/path/subpath?query#fragment", _)).
test(URI_81) :- \+(uri_parse(":/path/subpath?query", _)).
test(URI_82) :- \+(uri_parse(":/path/subpath#fragment", _)).
test(URI_83) :- \+(uri_parse(":/path/subpath", _)).
test(URI_84) :- \+(uri_parse(":/path?query#fragment", _)).
test(URI_85) :- \+(uri_parse(":/path?query", _)).
test(URI_86) :- \+(uri_parse(":/path#fragment", _)).
test(URI_87) :- \+(uri_parse(":/path", _)).
test(URI_88) :- \+(uri_parse(":/?query#fragment", _)).
test(URI_89) :- \+(uri_parse(":/#fragment", _)).
test(URI_90) :- \+(uri_parse(":/?query", _)).
test(URI_91) :- \+(uri_parse(":/", _)).

test(URI_92) :- \+(uri_parse(":path/subpath?query#fragment", _)).
test(URI_93) :- \+(uri_parse(":path/subpath?query", _)).
test(URI_94) :- \+(uri_parse(":path/subpath#fragment", _)).
test(URI_95) :- \+(uri_parse(":path/subpath", _)).
test(URI_96) :- \+(uri_parse(":path?query#fragment", _)).
test(URI_97) :- \+(uri_parse(":path?query", _)).
test(URI_98) :- \+(uri_parse(":path#fragment", _)).
test(URI_99) :- \+(uri_parse(":path", _)).
test(URI_100) :- \+(uri_parse(":?query#fragment", _)).
test(URI_101) :- \+(uri_parse(":#fragment", _)).
test(URI_102) :- \+(uri_parse(":?query", _)).
test(URI_103) :- \+(uri_parse(":", _)).

:- end_tests(uri_parse).

:- run_tests.