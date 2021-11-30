:- use_module(['uri-parse.pl']).
:- style_check(-singleton).

:- begin_tests(uri_parse).
:- set_prolog_flag(double_quotes, chars).

% TEST SCHEMA
test(schema) :- uri_parse("http://google.com", uri("http", _, _, _, _, _, _)).
test(schema) :- uri_parse("h11ps://google.com", uri("h11ps", _, _, _, _, _, _)).
test(schema) :- uri_parse("_http_://google.com", uri("_http_", _, _, _, _, _, _)).

% TEST USERINFO
test(userinfo) :- uri_parse("http://userinfo@prova", uri(_, "userinfo", "prova", _, _, _, _)).

:- end_tests(uri_parse).