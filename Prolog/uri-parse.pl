%%%% -*- Mode: Prolog -*-

% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- set_prolog_flag(double_quotes, chars).

uri_parse(URIStr, uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) :-
    uri_parse_(URIStr, 
        uri(uristructure(uri_scheme(Scheme), 
        uri_authority(uri_userinfo(UserInfo), 
        uri_host(Host), uri_port(Port)), 
        uri_path(Path), uri_query(Query), 
        uri_fragment(Fragment)))
    ).

uri_parse_(URIString, uri(URI)) :-
    phrase(uri(URI), URIString).

uri(uristructure(Scheme, Authority, Path, Query, Fragment)) -->
    scheme(Scheme),
    authority(Authority),
    path(Path),
    query(Query),
    fragment(Fragment),
    !.

% "news" ‘:’ host 
uri(uristructure(Scheme, uri_authority(uri_userinfo(""), Host, uri_port("")), uri_path(""), uri_query(""), uri_fragment(""))) -->
    scheme(Scheme),
    host_aux(Host),
    {string_chars(NewsString, "news"), Scheme = uri_scheme(NewsString)},
    !.

% "mailto" ‘:’ userinfo ['@'' host] 
uri(uristructure(Scheme, uri_authority(UserInfo, Host, uri_port("")), uri_path(""), uri_query(""), uri_fragment(""))) -->
    scheme(Scheme),
    userinfo(UserInfo),
    host_aux(Host),
    {string_chars(MailtoString, "mailto"), Scheme = uri_scheme(MailtoString)},
    !.

% scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
uri(uristructure(Scheme, "", Path, Query, Fragment)) -->
    scheme(Scheme),
    path(Path),
    query(Query),
    fragment(Fragment),
    !.

authority(uri_authority(UserInfo, Host, Port)) -->
    [/, /],
    userinfo(UserInfo),
    host_aux(Host),
    port(Port).

scheme(uri_scheme(Scheme)) --> 
    identificator(SchemeList),
    [:],
    { string_chars(Scheme, SchemeList) }.

identificator([H | T]) -->
    [H],
    { valid_char(H) },
    identificator(T),
    !.
identificator([X | []]) --> [X], { valid_char(X) }.

userinfo(uri_userinfo(UserInfo)) -->
    identificator(UserInfoList),
    [@],
    { string_chars(UserInfo, UserInfoList) },
    !.
userinfo(uri_userinfo([])) --> [].

host_aux(uri_host(Ip)) -->
    ip(IpList),
    { string_chars(Ip, IpList) },
    !.
host_aux(uri_host(Host)) -->
    host(HostList),
    { string_chars(Host, HostList) },
    !.

host(X) -->
    identificator_host(A),
    [.],
    host(B),
    {flatten([[A | [.]], B], X)},
    !.
host(X) -->
    identificator_host(X).

valid_char(X) :-
    char_type(X, alpha).

identificator_host([H | T]) -->
    [H],
    { valid_char(H), H \= '.'},
    identificator_host(T),
    !.
identificator_host([X | []]) --> [X], { valid_char(X), X \= '.'}.

port(uri_port(Port)) -->
    [:],
    digits(PortList),
    {string_chars(Port, PortList)},
    !.
port(uri_port([])) --> [].

fragment(uri_fragment(Fragment)) -->
    fragment_aux(FragmentList),
    {flatten(FragmentList, FlattenFragment)},
    {string_chars(Fragment, FlattenFragment)}.

fragment_aux(FragmentList) -->
    [#],
    identificator(FragmentList),
    !.
fragment_aux([]) --> [].

query(uri_query(Query)) -->
    query_aux(QueryList),
    {flatten(QueryList, FlattenQuery)},
    {string_chars(Query, FlattenQuery)}.

query_aux(QueryList) -->
    [?],
    identificator(QueryList),
    !.
query_aux([]) --> [].

path(uri_path(Path)) -->
    path_aux(PathList),
    {flatten(PathList, FlattenPath)},
    {string_chars(Path, FlattenPath)}.

% path_aux//
% Parse the path starting with /
path_aux(PathList) -->
    [/],
    identificator(A),
    path_aux(B),
    {PathList = [[/ | A], B]},
    !.
path_aux(PathList) -->
    [/],
    identificator(A),
    {PathList = [/ | A]},
    !.
% special method to parse the second type of URI
% scheme : path
path_aux(PathList) -->
    identificator(A),
    path_aux(B),
    {PathList = [[/ | A], B]},
    !.
path_aux([]) --> [/], !.
path_aux([]) --> [].
    
ip(Ip) --> 
    triplets(A), [.], triplets(B), [.], triplets(C), [.], triplets(D), 
    { flatten([A, '.', B, '.', C, '.', D], Ip) }.

triplets(X) --> 
    digit(A), digit(B), digit(C),
    {
        X = [A, B, C],
        string_chars(Str, X),
        number_string(Num, Str),
        between(0, 255, Num)
    }.


digit(X) --> [X], { is_digit(X) }.

digits([X | Xs]) -->
    digit(X),
    digits(Xs).
digits([X | []]) --> digit(X), !.