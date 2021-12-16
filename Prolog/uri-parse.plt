:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri('http', [], 'google.com', '80', [], [], [])).
test(schema2) :- uri_parse("h11ps://google.com", uri('h11ps', [], 'google.com', '80', [], [], [])).
test(schema3) :- uri_parse("_http_://google.com", uri('_http_', [], 'google.com', '80', [], [], [])).
test(schema_1) :- \+(uri_parse("://google.com", _)).
test(schema_2) :- \+(uri_parse("@://google.com", _)).
test(schema_3) :- \+(uri_parse("h:h://google.com", _)).
test(schema_4) :- \+(uri_parse("/hh://google.com", _)).
test(schema_5) :- \+(uri_parse("h#h://google.com", _)).
test(schema_6) :- \+(uri_parse("h?h://google.com", _)).
test(schema_7) :- \+(uri_parse(":://google.com", _)).

% TEST SCHEMA MAILTO
test(mailto1) :- uri_parse("mailto:userinfo", uri('mailto', 'userinfo', [], [], [], [], [])).
test(mailto2) :- uri_parse("mailto:userinfo@host", uri('mailto', 'userinfo', 'host', [], [], [], [])).
test(mailto_1) :- \+(uri_parse("mailto:", _)).
test(mailto_2) :- \+(uri_parse("mailto:userinfo@", _)).
test(mailto_3) :- \+(uri_parse("mailto:userinfo@host?query", _)).
test(mailto_3) :- \+(uri_parse("mailto:userinfo@host/path", _)).

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
test(news_1) :- \+(uri_parse("news:ho st", _)).
test(news_2) :- \+(uri_parse("news:ho/st", _)).
test(news_3) :- \+(uri_parse("news:host/path", _)).
test(news_4) :- \+(uri_parse("news:host/path?query", _)).
test(news_5) :- \+(uri_parse("news:host/path?query#fragment", _)).
test(news_6) :- \+(uri_parse("news:host:80", _)).
test(news_7) :- \+(uri_parse("news:userinfo@host", _)).

% TEST SCHEMA ZOS
test(zos1) :- uri_parse("zos://host/id44(id8)", uri('zos', [], 'host', '80', 'id44(id8)', [], [])).
test(zos2) :- uri_parse("zos://userinfo@host/id44(id8)", uri('zos', 'userinfo', 'host', '80', 'id44(id8)', [], [])).
test(zos3) :- uri_parse("zos://userinfo@host:4832/id44(id8)", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', [], [])).
test(zos4) :- uri_parse("zos://userinfo@host:4832/id44(id8)?query", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', 'query', [])).
test(zos5) :- uri_parse("zos://userinfo@host:4832/id44(id8)?query#fragment", uri('zos', 'userinfo', 'host', '4832', 'id44(id8)', 'query', 'fragment')).
test(zos6) :- uri_parse("zos://host/id.44(id8)", uri('zos', [], 'host', '80', 'id.44(id8)', [], [])).
test(zos7) :- uri_parse("zos://host/i.d.4.4(id8)", uri('zos', [], 'host', '80', 'i.d.4.4(id8)', [], [])).
test(zos8) :- uri_parse("zos://host/i.d.4.4", uri('zos', [], 'host', '80', 'i.d.4.4', [], [])).
test(zos9) :- uri_parse("zos://host/id..prova", uri('zos', [], 'host', '80', 'id..prova', [], [])).
test(zos9) :- uri_parse("zos://host/id..prova(id8)", uri('zos', [], 'host', '80', 'id..prova(id8)', [], [])).
test(zos_1) :- \+(uri_parse("zos://host", _)).
test(zos_2) :- \+(uri_parse("zos://host/.i.d", _)).
test(zos_3) :- \+(uri_parse("zos://host/.", _)).
test(zos_4) :- \+(uri_parse("zos://host/", _)).
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
test(zos_21) :- \+(uri_parse("zos://host/id..prova..(id8)", _)).
test(zos_22) :- \+(uri_parse("zos://host/..id..prova(id8)", _)).
test(zos_23) :- \+(uri_parse("zos://host/.i.d(id8)", _)).
test(zos_24) :- \+(uri_parse("zos://host/.(id8)", _)).
test(zos_25) :- \+(uri_parse("zos://host/id.(id8)", _)).


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
test(host1) :- uri_parse("scheme://host", uri('scheme', [], 'host', '80', [], [], [])).
test(host2) :- uri_parse("scheme://userinfo@host", uri('scheme', 'userinfo', 'host', '80', [], [], [])).
test(host3) :- uri_parse("scheme://host:123", uri('scheme', [], 'host', '123', [], [], [])).
test(host4) :- uri_parse("scheme://userinfo@host:123", uri('scheme', 'userinfo', 'host', '123', [], [], [])).
test(host5) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', '80', 'path', [], [])).
test(host6) :- uri_parse("scheme://userinfo@host.com:123", uri('scheme', 'userinfo', 'host.com', '123', [], [], [])).
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
test(path1) :- uri_parse("scheme://host/path", uri('scheme', [], 'host', '80', 'path', [], [])).
test(path2) :- uri_parse("scheme://host/path/prova", uri('scheme', [], 'host', '80', 'path/prova', [], [])).
test(path3) :- uri_parse("scheme://host/", uri('scheme', [], 'host', '80', [], [], [])).
test(path4) :- uri_parse("scheme://host", uri('scheme', [], 'host', '80', [], [], [])).
test(path5) :- uri_parse("scheme://host/path?query", uri('scheme', [], 'host', '80', 'path', 'query', [])).
test(path6) :- uri_parse("scheme://host/pro va", uri('scheme', [], 'host', '80', 'pro%20va', [], [])).
test(path7) :- uri_parse("scheme://host/pro.va", uri('scheme', [], 'host', '80', 'pro.va', [], [])).
test(path7) :- uri_parse("scheme://host/path#fragment", uri('scheme', [], 'host', '80', 'path', [], 'fragment')).
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
test(query1) :- uri_parse("scheme://host/?query", uri('scheme', [], 'host', '80', [], 'query', [])).
test(query2) :- uri_parse("scheme://host/?qu ery", uri('scheme', [], 'host', '80', [], 'qu%20ery', [])).
test(query3) :- uri_parse("scheme://host/?qu.ery", uri('scheme', [], 'host', '80', [], 'qu.ery', [])).
test(query4) :- uri_parse("scheme://host/?qu:ery", uri('scheme', [], 'host', '80', [], 'qu:ery', [])).
test(query5) :- uri_parse("scheme://host/?qu@ery", uri('scheme', [], 'host', '80', [], 'qu@ery', [])).
test(query6) :- uri_parse("scheme://host/?qu/ery", uri('scheme', [], 'host', '80', [], 'qu/ery', [])).
test(query7) :- uri_parse("scheme://host/?qu?ery", uri('scheme', [], 'host', '80', [], 'qu?ery', [])).
test(query8) :- uri_parse("scheme://host", uri('scheme', [], 'host', '80', [], [], [])).
test(query9) :- uri_parse("scheme://host/", uri('scheme', [], 'host', '80', [], [], [])).
test(query10) :- uri_parse("scheme://host/path?query#fragment", uri('scheme', [], 'host', '80', 'path', 'query', 'fragment')).
test(query_1) :- \+(uri_parse("scheme://host/?", _)).
test(query_2) :- \+(uri_parse("scheme://host/?#", _)).
test(query_3) :- \+(uri_parse("scheme://host?query", _)).

% TEST FRAGMENT
test(fragment1) :- uri_parse("scheme://host/#frag", uri('scheme', [], 'host', '80', [], [], 'frag')).
test(fragment2) :- uri_parse("scheme://host/path?query#frag", uri('scheme', [], 'host', '80', 'path', 'query', 'frag')).
test(fragment3) :- uri_parse("scheme://host/?query#frag", uri('scheme', [], 'host', '80', [], 'query', 'frag')).
test(fragment4) :- uri_parse("scheme://host", uri('scheme', [], 'host', '80', [], [], [])).
test(fragment_1) :- \+(uri_parse("scheme://host/#", _)).
test(fragment_2) :- \+(uri_parse("scheme://host#", _)).

% TEST INTEGRATION ---- PRIMO TIPO DI URI
test(primoTipo1) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', '123', 'path/subpath', 'query', 'fragment')).
test(primoTipo2) :- uri_parse("http://userinfo@host.subhost:123/path/subpath?query", uri('http', 'userinfo', 'host.subhost', '123', 'path/subpath', 'query', [])).
test(primoTipo3) :- uri_parse("http://userinfo@host.subhost:123/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', '123', 'path/subpath', [], 'fragment')).
test(primoTIpo4) :- uri_parse("http://userinfo@host.subhost:123/path/subpath", uri('http', 'userinfo', 'host.subhost', '123', 'path/subpath', [], [])).
test(primoTipo5) :- uri_parse("http://userinfo@host.subhost:123/path?query#fragment", uri('http', 'userinfo', 'host.subhost', '123', 'path', 'query', 'fragment')).
test(primoTipo6) :- uri_parse("http://userinfo@host.subhost:123/path?query", uri('http', 'userinfo', 'host.subhost', '123', 'path', 'query', [])).
test(primoTipo7) :- uri_parse("http://userinfo@host.subhost:123/path#fragment", uri('http', 'userinfo', 'host.subhost', '123', 'path', [], 'fragment')).
test(primoTipo8) :- uri_parse("http://userinfo@host.subhost:123/path", uri('http', 'userinfo', 'host.subhost', '123', 'path', [], [])).
test(primoTipo9) :- uri_parse("http://userinfo@host.subhost:123/?query#fragment", uri('http', 'userinfo', 'host.subhost', '123', [], 'query', 'fragment')).
test(primoTipo10) :- uri_parse("http://userinfo@host.subhost:123/?query", uri('http', 'userinfo', 'host.subhost', '123', [], 'query', [])).
test(primoTipo11) :- uri_parse("http://userinfo@host.subhost:123/#fragment", uri('http', 'userinfo', 'host.subhost', '123', [], [], 'fragment')).
test(primoTipo12) :- uri_parse("http://userinfo@host.subhost:123/", uri('http', 'userinfo', 'host.subhost', '123', [], [], [])).
test(primoTipo13) :- uri_parse("http://userinfo@host.subhost:123", uri('http', 'userinfo', 'host.subhost', '123', [], [], [])).

test(primoTipo14) :- uri_parse("http://userinfo@host.subhost/path/subpath?query#fragment", uri('http', 'userinfo', 'host.subhost', '80', 'path/subpath', 'query', 'fragment')).
test(primoTipo15) :- uri_parse("http://userinfo@host.subhost/path/subpath?query", uri('http', 'userinfo', 'host.subhost', '80', 'path/subpath', 'query', [])).
test(primoTipo16) :- uri_parse("http://userinfo@host.subhost/path/subpath#fragment", uri('http', 'userinfo', 'host.subhost', '80', 'path/subpath', [], 'fragment')).
test(primoTIpo17) :- uri_parse("http://userinfo@host.subhost/path/subpath", uri('http', 'userinfo', 'host.subhost', '80', 'path/subpath', [], [])).
test(primoTipo18) :- uri_parse("http://userinfo@host.subhost/path?query#fragment", uri('http', 'userinfo', 'host.subhost', '80', 'path', 'query', 'fragment')).
test(primoTipo19) :- uri_parse("http://userinfo@host.subhost/path?query", uri('http', 'userinfo', 'host.subhost', '80', 'path', 'query', [])).
test(primoTipo20) :- uri_parse("http://userinfo@host.subhost/path#fragment", uri('http', 'userinfo', 'host.subhost', '80', 'path', [], 'fragment')).
test(primoTipo21) :- uri_parse("http://userinfo@host.subhost/path", uri('http', 'userinfo', 'host.subhost', '80', 'path', [], [])).
test(primoTipo22) :- uri_parse("http://userinfo@host.subhost/?query#fragment", uri('http', 'userinfo', 'host.subhost', '80', [], 'query', 'fragment')).
test(primoTipo23) :- uri_parse("http://userinfo@host.subhost/?query", uri('http', 'userinfo', 'host.subhost', '80', [], 'query', [])).
test(primoTipo24) :- uri_parse("http://userinfo@host.subhost/#fragment", uri('http', 'userinfo', 'host.subhost', '80', [], [], 'fragment')).
test(primoTipo25) :- uri_parse("http://userinfo@host.subhost/", uri('http', 'userinfo', 'host.subhost', '80', [], [], [])).
test(primoTipo26) :- uri_parse("http://userinfo@host.subhost", uri('http', 'userinfo', 'host.subhost', '80', [], [], [])).

test(primoTipo27) :- uri_parse("http://host.subhost:123/path/subpath?query#fragment", uri('http', [], 'host.subhost', '123', 'path/subpath', 'query', 'fragment')).
test(primoTipo28) :- uri_parse("http://host.subhost:123/path/subpath?query", uri('http', [], 'host.subhost', '123', 'path/subpath', 'query', [])).
test(primoTipo29) :- uri_parse("http://host.subhost:123/path/subpath#fragment", uri('http', [], 'host.subhost', '123', 'path/subpath', [], 'fragment')).
test(primoTIpo30) :- uri_parse("http://host.subhost:123/path/subpath", uri('http', [], 'host.subhost', '123', 'path/subpath', [], [])).
test(primoTipo31) :- uri_parse("http://host.subhost:123/path?query#fragment", uri('http', [], 'host.subhost', '123', 'path', 'query', 'fragment')).
test(primoTipo32) :- uri_parse("http://host.subhost:123/path?query", uri('http', [], 'host.subhost', '123', 'path', 'query', [])).
test(primoTipo33) :- uri_parse("http://host.subhost:123/path#fragment", uri('http', [], 'host.subhost', '123', 'path', [], 'fragment')).
test(primoTipo34) :- uri_parse("http://host.subhost:123/path", uri('http', [], 'host.subhost', '123', 'path', [], [])).
test(primoTipo35) :- uri_parse("http://host.subhost:123/?query#fragment", uri('http', [], 'host.subhost', '123', [], 'query', 'fragment')).
test(primoTipo36) :- uri_parse("http://host.subhost:123/?query", uri('http', [], 'host.subhost', '123', [], 'query', [])).
test(primoTipo37) :- uri_parse("http://host.subhost:123/#fragment", uri('http', [], 'host.subhost', '123', [], [], 'fragment')).
test(primoTipo38) :- uri_parse("http://host.subhost:123/", uri('http', [], 'host.subhost', '123', [], [], [])).
test(primoTipo39) :- uri_parse("http://host.subhost:123", uri('http', [], 'host.subhost', '123', [], [], [])).

test(primoTipo40) :- uri_parse("http://host.subhost/path/subpath?query#fragment", uri('http', [], 'host.subhost', '80', 'path/subpath', 'query', 'fragment')).
test(primoTipo41) :- uri_parse("http://host.subhost/path/subpath?query", uri('http', [], 'host.subhost', '80', 'path/subpath', 'query', [])).
test(primoTipo42) :- uri_parse("http://host.subhost/path/subpath#fragment", uri('http', [], 'host.subhost', '80', 'path/subpath', [], 'fragment')).
test(primoTIpo43) :- uri_parse("http://host.subhost/path/subpath", uri('http', [], 'host.subhost', '80', 'path/subpath', [], [])).
test(primoTipo44) :- uri_parse("http://host.subhost/path?query#fragment", uri('http', [], 'host.subhost', '80', 'path', 'query', 'fragment')).
test(primoTipo45) :- uri_parse("http://host.subhost/path?query", uri('http', [], 'host.subhost', '80', 'path', 'query', [])).
test(primoTipo46) :- uri_parse("http://host.subhost/path#fragment", uri('http', [], 'host.subhost', '80', 'path', [], 'fragment')).
test(primoTipo47) :- uri_parse("http://host.subhost/path", uri('http', [], 'host.subhost', '80', 'path', [], [])).
test(primoTipo48) :- uri_parse("http://host.subhost/?query#fragment", uri('http', [], 'host.subhost', '80', [], 'query', 'fragment')).
test(primoTipo49) :- uri_parse("http://host.subhost/?query", uri('http', [], 'host.subhost', '80', [], 'query', [])).
test(primoTipo50) :- uri_parse("http://host.subhost/#fragment", uri('http', [], 'host.subhost', '80', [], [], 'fragment')).
test(primoTipo51) :- uri_parse("http://host.subhost/", uri('http', [], 'host.subhost', '80', [], [], [])).
test(primoTipo52) :- uri_parse("http://host.subhost", uri('http', [], 'host.subhost', '80', [], [], [])).

test(primoTipo53) :- uri_parse("http://userinfo@host:123/path/subpath?query#fragment", uri('http', 'userinfo', 'host', '123', 'path/subpath', 'query', 'fragment')).
test(primoTipo54) :- uri_parse("http://userinfo@host:123/path/subpath?query", uri('http', 'userinfo', 'host', '123', 'path/subpath', 'query', [])).
test(primoTipo55) :- uri_parse("http://userinfo@host:123/path/subpath#fragment", uri('http', 'userinfo', 'host', '123', 'path/subpath', [], 'fragment')).
test(primoTIpo56) :- uri_parse("http://userinfo@host:123/path/subpath", uri('http', 'userinfo', 'host', '123', 'path/subpath', [], [])).
test(primoTipo57) :- uri_parse("http://userinfo@host:123/path?query#fragment", uri('http', 'userinfo', 'host', '123', 'path', 'query', 'fragment')).
test(primoTipo58) :- uri_parse("http://userinfo@host:123/path?query", uri('http', 'userinfo', 'host', '123', 'path', 'query', [])).
test(primoTipo59) :- uri_parse("http://userinfo@host:123/path#fragment", uri('http', 'userinfo', 'host', '123', 'path', [], 'fragment')).
test(primoTipo60) :- uri_parse("http://userinfo@host:123/path", uri('http', 'userinfo', 'host', '123', 'path', [], [])).
test(primoTipo61) :- uri_parse("http://userinfo@host:123/?query#fragment", uri('http', 'userinfo', 'host', '123', [], 'query', 'fragment')).
test(primoTipo62) :- uri_parse("http://userinfo@host:123/?query", uri('http', 'userinfo', 'host', '123', [], 'query', [])).
test(primoTipo63) :- uri_parse("http://userinfo@host:123/#fragment", uri('http', 'userinfo', 'host', '123', [], [], 'fragment')).
test(primoTipo64) :- uri_parse("http://userinfo@host:123/", uri('http', 'userinfo', 'host', '123', [], [], [])).
test(primoTipo65) :- uri_parse("http://userinfo@host:123", uri('http', 'userinfo', 'host', '123', [], [], [])).

test(primoTipo66) :- uri_parse("http://userinfo@host/path/subpath?query#fragment", uri('http', 'userinfo', 'host', '80', 'path/subpath', 'query', 'fragment')).
test(primoTipo67) :- uri_parse("http://userinfo@host/path/subpath?query", uri('http', 'userinfo', 'host', '80', 'path/subpath', 'query', [])).
test(primoTipo68) :- uri_parse("http://userinfo@host/path/subpath#fragment", uri('http', 'userinfo', 'host', '80', 'path/subpath', [], 'fragment')).
test(primoTIpo69) :- uri_parse("http://userinfo@host/path/subpath", uri('http', 'userinfo', 'host', '80', 'path/subpath', [], [])).
test(primoTipo70) :- uri_parse("http://userinfo@host/path?query#fragment", uri('http', 'userinfo', 'host', '80', 'path', 'query', 'fragment')).
test(primoTipo71) :- uri_parse("http://userinfo@host/path?query", uri('http', 'userinfo', 'host', '80', 'path', 'query', [])).
test(primoTipo72) :- uri_parse("http://userinfo@host/path#fragment", uri('http', 'userinfo', 'host', '80', 'path', [], 'fragment')).
test(primoTipo73) :- uri_parse("http://userinfo@host/path", uri('http', 'userinfo', 'host', '80', 'path', [], [])).
test(primoTipo74) :- uri_parse("http://userinfo@host/?query#fragment", uri('http', 'userinfo', 'host', '80', [], 'query', 'fragment')).
test(primoTipo75) :- uri_parse("http://userinfo@host/?query", uri('http', 'userinfo', 'host', '80', [], 'query', [])).
test(primoTipo76) :- uri_parse("http://userinfo@host/#fragment", uri('http', 'userinfo', 'host', '80', [], [], 'fragment')).
test(primoTipo77) :- uri_parse("http://userinfo@host/", uri('http', 'userinfo', 'host', '80', [], [], [])).
test(primoTipo78) :- uri_parse("http://userinfo@host", uri('http', 'userinfo', 'host', '80', [], [], [])).

test(primoTipo79) :- uri_parse("http://host:123/path/subpath?query#fragment", uri('http', [], 'host', '123', 'path/subpath', 'query', 'fragment')).
test(primoTipo80) :- uri_parse("http://host:123/path/subpath?query", uri('http', [], 'host', '123', 'path/subpath', 'query', [])).
test(primoTipo81) :- uri_parse("http://host:123/path/subpath#fragment", uri('http', [], 'host', '123', 'path/subpath', [], 'fragment')).
test(primoTIpo82) :- uri_parse("http://host:123/path/subpath", uri('http', [], 'host', '123', 'path/subpath', [], [])).
test(primoTipo83) :- uri_parse("http://host:123/path?query#fragment", uri('http', [], 'host', '123', 'path', 'query', 'fragment')).
test(primoTipo84) :- uri_parse("http://host:123/path?query", uri('http', [], 'host', '123', 'path', 'query', [])).
test(primoTipo85) :- uri_parse("http://host:123/path#fragment", uri('http', [], 'host', '123', 'path', [], 'fragment')).
test(primoTipo86) :- uri_parse("http://host:123/path", uri('http', [], 'host', '123', 'path', [], [])).
test(primoTipo87) :- uri_parse("http://host:123/?query#fragment", uri('http', [], 'host', '123', [], 'query', 'fragment')).
test(primoTipo88) :- uri_parse("http://host:123/?query", uri('http', [], 'host', '123', [], 'query', [])).
test(primoTipo89) :- uri_parse("http://host:123/#fragment", uri('http', [], 'host', '123', [], [], 'fragment')).
test(primoTipo90) :- uri_parse("http://host:123/", uri('http', [], 'host', '123', [], [], [])).
test(primoTipo91) :- uri_parse("http://host:123", uri('http', [], 'host', '123', [], [], [])).

test(primoTipo92) :- uri_parse("http://host/path/subpath?query#fragment", uri('http', [], 'host', '80', 'path/subpath', 'query', 'fragment')).
test(primoTipo93) :- uri_parse("http://host/path/subpath?query", uri('http', [], 'host', '80', 'path/subpath', 'query', [])).
test(primoTipo94) :- uri_parse("http://host/path/subpath#fragment", uri('http', [], 'host', '80', 'path/subpath', [], 'fragment')).
test(primoTIpo95) :- uri_parse("http://host/path/subpath", uri('http', [], 'host', '80', 'path/subpath', [], [])).
test(primoTipo96) :- uri_parse("http://host/path?query#fragment", uri('http', [], 'host', '80', 'path', 'query', 'fragment')).
test(primoTipo97) :- uri_parse("http://host/path?query", uri('http', [], 'host', '80', 'path', 'query', [])).
test(primoTipo98) :- uri_parse("http://host/path#fragment", uri('http', [], 'host', '80', 'path', [], 'fragment')).
test(primoTipo99) :- uri_parse("http://host/path", uri('http', [], 'host', '80', 'path', [], [])).
test(primoTipo100) :- uri_parse("http://host/?query#fragment", uri('http', [], 'host', '80', [], 'query', 'fragment')).
test(primoTipo101) :- uri_parse("http://host/?query", uri('http', [], 'host', '80', [], 'query', [])).
test(primoTipo102) :- uri_parse("http://host/#fragment", uri('http', [], 'host', '80', [], [], 'fragment')).
test(primoTipo103) :- uri_parse("http://host/", uri('http', [], 'host', '80', [], [], [])).
test(primoTipo104) :- uri_parse("http://host", uri('http', [], 'host', '80', [], [], [])).

test(primoTIpo_1) :- \+(uri_parse("http://", _)).
test(primoTipo_2) :- \+(uri_parse("http:///path/subpath?query#fragment", _)).
test(primoTipo_3) :- \+(uri_parse("http:///path/subpath?query", _)).
test(primoTipo_4) :- \+(uri_parse("http:///path/subpath#fragment", _)).
test(primoTIpo_5) :- \+(uri_parse("http:///path/subpath", _)).
test(primoTipo_6) :- \+(uri_parse("http:///path?query#fragment", _)).
test(primoTipo_7) :- \+(uri_parse("http:///path?query", _)).
test(primoTipo_8) :- \+(uri_parse("http:///path#fragment", _)).
test(primoTipo_9) :- \+(uri_parse("http:///path", _)).
test(primoTipo_10) :- \+(uri_parse("http:///?query#fragment", _)).
test(primoTipo_11) :- \+(uri_parse("http:///?query", _)).
test(primoTipo_12) :- \+(uri_parse("http:///#fragment", _)).
test(primoTipo_13) :- \+(uri_parse("http:///", _)).
test(primoTipo_14) :- \+(uri_parse("http://", _)).
test(primoTipo_15) :- \+(uri_parse("http://host?query#fragment", _)).
test(primoTipo_16) :- \+(uri_parse("http://host?query", _)).
test(primoTipo_17) :- \+(uri_parse("http://host#fragment", _)).
test(primoTipo_18) :- \+(uri_parse("://host/path/subpath?query#fragment", _)).
test(primoTipo_19) :- \+(uri_parse("://host/path/subpath?query", _)).
test(primoTipo_20) :- \+(uri_parse("://host/path/subpath#fragment", _)).
test(primoTIpo_21) :- \+(uri_parse("://host/path/subpath", _)).
test(primoTipo_22) :- \+(uri_parse("://host/path?query#fragment", _)).
test(primoTipo_23) :- \+(uri_parse("://host/path?query", _)).
test(primoTipo_24) :- \+(uri_parse("://host/path#fragment", _)).
test(primoTipo_25) :- \+(uri_parse("://host/path", _)).
test(primoTipo_26) :- \+(uri_parse("://host/?query#fragment", _)).
test(primoTipo_27) :- \+(uri_parse("://host/?query", _)).
test(primoTipo_28) :- \+(uri_parse("://host/#fragment", _)).
test(primoTipo_29) :- \+(uri_parse("://host/", _)).
test(primoTipo_30) :- \+(uri_parse("://host", _)).
test(primoTipo_31) :- \+(uri_parse("http//host/path/subpath?query#fragment", _)).
test(primoTipo_32) :- \+(uri_parse("http//host/path/subpath?query", _)).
test(primoTipo_33) :- \+(uri_parse("http//host/path/subpath#fragment", _)).
test(primoTIpo_34) :- \+(uri_parse("http//host/path/subpath", _)).
test(primoTipo_35) :- \+(uri_parse("http//host/path?query#fragment", _)).
test(primoTipo_36) :- \+(uri_parse("http//host/path?query", _)).
test(primoTipo_37) :- \+(uri_parse("http//host/path#fragment", _)).
test(primoTipo_38) :- \+(uri_parse("http//host/path", _)).
test(primoTipo_39) :- \+(uri_parse("http//host/?query#fragment", _)).
test(primoTipo_40) :- \+(uri_parse("http//host/?query", _)).
test(primoTipo_41) :- \+(uri_parse("http//host/#fragment", _)).
test(primoTipo_42) :- \+(uri_parse("http//host/", _)).
test(primoTipo_43) :- \+(uri_parse("http//host", _)).




% INTEGRATION TEST ---- secondo tipo di uri
test(secondoTipo1) :- uri_parse("http:/path/subpath?query#fragment", uri('http', [], [], [], 'path/subpath', 'query', 'fragment')).
test(secondoTipo2) :- uri_parse("http:/path/subpath?query", uri('http', [], [], [], 'path/subpath', 'query', [])).
test(secondoTipo3) :- uri_parse("http:/path/subpath#fragment", uri('http', [], [], [], 'path/subpath', [], 'fragment')).
test(secondoTipo4) :- uri_parse("http:/path/subpath", uri('http', [], [], [], 'path/subpath', [], [])).
test(secondoTipo5) :- uri_parse("http:/path?query#fragment", uri('http', [], [], [], 'path', 'query', 'fragment')).
test(secondoTipo6) :- uri_parse("http:/path?query", uri('http', [], [], [], 'path', 'query', [])).
test(secondoTipo7) :- uri_parse("http:/path#fragment", uri('http', [], [], [], 'path', [], 'fragment')).
test(secondoTipo8) :- uri_parse("http:/path", uri('http', [], [], [], 'path', [], [])).
test(secondoTipo9) :- uri_parse("http:/?query#fragment", uri('http', [], [], [], [], 'query', 'fragment')).
test(secondoTipo10) :- uri_parse("http:/#fragment", uri('http', [], [], [], [], [], 'fragment')).
test(secondoTipo11) :- uri_parse("http:/?query", uri('http', [], [], [], [], 'query', [])).
test(secondoTipo12) :- uri_parse("http:/", uri('http', [], [], [], [], [], [])).

test(secondoTipo13) :- uri_parse("http:path/subpath?query#fragment", uri('http', [], [], [], 'path/subpath', 'query', 'fragment')).
test(secondoTipo14) :- uri_parse("http:path/subpath?query", uri('http', [], [], [], 'path/subpath', 'query', [])).
test(secondoTipo15) :- uri_parse("http:path/subpath#fragment", uri('http', [], [], [], 'path/subpath', [], 'fragment')).
test(secondoTipo16) :- uri_parse("http:path/subpath", uri('http', [], [], [], 'path/subpath', [], [])).
test(secondoTipo17) :- uri_parse("http:path?query#fragment", uri('http', [], [], [], 'path', 'query', 'fragment')).
test(secondoTipo18) :- uri_parse("http:path?query", uri('http', [], [], [], 'path', 'query', [])).
test(secondoTipo19) :- uri_parse("http:path#fragment", uri('http', [], [], [], 'path', [], 'fragment')).
test(secondoTipo20) :- uri_parse("http:path", uri('http', [], [], [], 'path', [], [])).
test(secondoTipo21) :- uri_parse("http:?query#fragment", uri('http', [], [], [], [], 'query', 'fragment')).
test(secondoTipo22) :- uri_parse("http:#fragment", uri('http', [], [], [], [], [], 'fragment')).
test(secondoTipo23) :- uri_parse("http:?query", uri('http', [], [], [], [], 'query', [])).
test(secondoTipo24) :- uri_parse("http:", uri('http', [], [], [], [], [], [])).

test(secondoTipo_1) :- \+(uri_parse("http/path/subpath?query#fragment", _)).
test(secondoTipo_2) :- \+(uri_parse("http/path/subpath?query", _)).
test(secondoTipo_3) :- \+(uri_parse("http/path/subpath#fragment", _)).
test(secondoTipo_4) :- \+(uri_parse("http/path/subpath", _)).
test(secondoTipo_5) :- \+(uri_parse("http/path?query#fragment", _)).
test(secondoTipo_6) :- \+(uri_parse("http/path?query", _)).
test(secondoTipo_7) :- \+(uri_parse("http/path#fragment", _)).
test(secondoTipo_8) :- \+(uri_parse("http/path", _)).
test(secondoTipo_9) :- \+(uri_parse("http/?query#fragment", _)).
test(secondoTipo_10) :- \+(uri_parse("http/#fragment", _)).
test(secondoTipo_11) :- \+(uri_parse("http/?query", _)).
test(secondoTipo_12) :- \+(uri_parse("http/", _)).

test(secondoTipo_13) :- \+(uri_parse("httppath/subpath?query#fragment", _)).
test(secondoTipo_14) :- \+(uri_parse("httppath/subpath?query", _)).
test(secondoTipo_15) :- \+(uri_parse("httppath/subpath#fragment", _)).
test(secondoTipo_16) :- \+(uri_parse("httppath/subpath", _)).
test(secondoTipo_17) :- \+(uri_parse("httppath?query#fragment", _)).
test(secondoTipo_18) :- \+(uri_parse("httppath?query", _)).
test(secondoTipo_19) :- \+(uri_parse("httppath#fragment", _)).
test(secondoTipo_20) :- \+(uri_parse("httppath", _)).
test(secondoTipo_21) :- \+(uri_parse("http?query#fragment", _)).
test(secondoTipo_22) :- \+(uri_parse("http#fragment", _)).
test(secondoTipo_23) :- \+(uri_parse("http?query", _)).
test(secondoTipo_24) :- \+(uri_parse("http", _)).

test(secondoTipo_25) :- \+(uri_parse(":/path/subpath?query#fragment", _)).
test(secondoTipo_26) :- \+(uri_parse(":/path/subpath?query", _)).
test(secondoTipo_27) :- \+(uri_parse(":/path/subpath#fragment", _)).
test(secondoTipo_28) :- \+(uri_parse(":/path/subpath", _)).
test(secondoTipo_29) :- \+(uri_parse(":/path?query#fragment", _)).
test(secondoTipo_30) :- \+(uri_parse(":/path?query", _)).
test(secondoTipo_31) :- \+(uri_parse(":/path#fragment", _)).
test(secondoTipo_32) :- \+(uri_parse(":/path", _)).
test(secondoTipo_33) :- \+(uri_parse(":/?query#fragment", _)).
test(secondoTipo_34) :- \+(uri_parse(":/#fragment", _)).
test(secondoTipo_35) :- \+(uri_parse(":/?query", _)).
test(secondoTipo_36) :- \+(uri_parse(":/", _)).

test(secondoTipo_37) :- \+(uri_parse(":path/subpath?query#fragment", _)).
test(secondoTipo_38) :- \+(uri_parse(":path/subpath?query", _)).
test(secondoTipo_39) :- \+(uri_parse(":path/subpath#fragment", _)).
test(secondoTipo_40) :- \+(uri_parse(":path/subpath", _)).
test(secondoTipo_41) :- \+(uri_parse(":path?query#fragment", _)).
test(secondoTipo_42) :- \+(uri_parse(":path?query", _)).
test(secondoTipo_43) :- \+(uri_parse(":path#fragment", _)).
test(secondoTipo_44) :- \+(uri_parse(":path", _)).
test(secondoTipo_45) :- \+(uri_parse(":?query#fragment", _)).
test(secondoTipo_46) :- \+(uri_parse(":#fragment", _)).
test(secondoTipo_47) :- \+(uri_parse(":?query", _)).
test(secondoTipo_48) :- \+(uri_parse(":", _)).

:- end_tests(uri_parse).