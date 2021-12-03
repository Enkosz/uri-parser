:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).
:- set_prolog_flag(double_quotes, chars).

% TEST SCHEMA
test(schema1) :- uri_parse("http://google.com", uri("http", _, _, _, _, _, _)).
test(schema2) :- uri_parse("h11ps://google.com", uri("h11ps", _, _, _, _, _, _)).
test(schema3) :- uri_parse("_http_://google.com", uri("_http_", _, _, _, _, _, _)).
test(schema4) :- \+(uri_parse("htt:p://google.com", uri(_, _, _, _, _, _, _))).

% TEST USERINFO
test(userinfo1) :- uri_parse("http://userinfo@host", uri(_, "userinfo", "host", _, _, _, _)).
test(userinfo2) :- uri_parse("http://user_info@host", uri(_, "user_info", "host", _, _, _, _)).
test(userinfo3) :- uri_parse("http://user123info@host", uri(_, "user123info", "host", _, _, _, _)).
test(userinfo4) :- \+(uri_parse("http://user@info@host", uri(_, _, _, _, _, _, _))).
test(userinfo5) :- \+(uri_parse("http://userin:fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo6) :- \+(uri_parse("http://userin/fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo7) :- \+(uri_parse("http://userin?fo@host", uri(_, _, _, _, _, _, _))).
test(userinfo8) :- \+(uri_parse("http://userin#fo@host", uri(_, _, _, _, _, _, _))).

% Quando si sistemerà il problema con "/" bisogna corregere il path nell'uri, in quanto non avrà "/" all'inizio

% TEST HOST
test(host1) :- uri_parse("scheme://host", uri(_, _, "host", _, _, _, _)).
test(host2) :- uri_parse("scheme://userinfo@host", uri(_, "userinfo", "host", _, _, _, _)).
test(host3) :- uri_parse("scheme://host:123", uri(_, _, "host", "123", _, _, _)).
test(host4) :- uri_parse("scheme://userinfo@host:123", uri(_, "userinfo", "host", "123", _, _, _)).
test(host5) :- uri_parse("scheme://host/path", uri(_, _, "host", _, "path", _, _)).
test(host6) :- uri_parse("scheme://userinfo@host.com:123", uri(_, "userinfo", "host.com", "123", _, _, _)).
test(host7) :- \+(uri_parse("scheme://userinfo@ho?st:123", uri(_, _, _, _, _, _, _))).
test(host8) :- \+(uri_parse("scheme://userinfo@ho@st:123", uri(_, _, _, _, _, _, _))).
test(host9) :- \+(uri_parse("scheme://userinfo@ho:st:123", uri(_, _, _, _, _, _, _))).
test(host10) :- \+(uri_parse("scheme://userinfo@ho/st:123", uri(_, _, _, _, _, _, _))).
test(host11) :- \+(uri_parse("scheme://userinfo@ho#st:123", uri(_, _, _, _, _, _, _))).

% TEST PORT
test(port1) :- uri_parse("scheme://host:123", uri(_, _, "host", "123", _, _, _)).
test(port2) :- uri_parse("scheme://host:123/path", uri(_, _, "host", "123", "path", _, _)).
test(port3) :- \+(uri_parse("scheme://host:1_23/path", uri(_, _, _, _, _, _, _))).
test(port4) :- \+(uri_parse("scheme://host:1a23/path", uri(_, _, _, _, _, _, _))).



:- end_tests(uri_parse).