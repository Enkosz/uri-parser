%%%% -*- Mode: Prolog -*-

% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- module(uri_parse, [uri_parse/2]).
:- set_prolog_flag(double_quotes, chars).

uri_parse(URIString, uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) :-
    uri_parse_(URIString, 
        uri(components(
            scheme(Scheme), 
            authority(userinfo(UserInfo), host(Host), port(_Port)),
            path(Path), query(Query), 
            fragment(Fragment)
        ))
              ),
    ( _Port = [] -> uri_default_port(Scheme, Port); Port = '' ).

uri_parse_(URIString, uri(URI)) :-
    phrase(uri(URI), URIString).

uri(components(Scheme, Authority, Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    uri_authority(Authority),
    uri_path(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    !.

% "news" ‘:’ host 
uri(components(Scheme, authority(userinfo(''), Host, port('')), path(''), query(''), fragment(''))) -->
    uri_scheme(Scheme),
    uri_host_aux(Host),
    {string_chars(NewsString, "news"), Scheme = scheme(NewsString)},
    !.

% ["fax" | "tel"] ‘:’ host 
uri(components(Scheme, authority(UserInfo, host(''), port('')), path(''), query(''), fragment(''))) -->
    uri_scheme(Scheme),
    uri_userinfo(UserInfo),
    {string_chars(TelString, "tel"), Scheme = scheme(TelString)}, !.

% "mailto" ‘:’ userinfo ['@'' host] 
uri(components(Scheme, authority(UserInfo, Host, port('')), path(''), query(''), fragment(''))) -->
    uri_scheme(Scheme),
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    {string_chars(MailtoString, "mailto"), Scheme = scheme(MailtoString)},
    !.

% scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
uri(components(Scheme, authority(userinfo(''), host(''), port('')), Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    uri_path(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    !.

uri_authority(authority(UserInfo, Host, Port)) -->
    [/, /],
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    uri_port(Port).

uri_scheme(scheme(Scheme)) --> 
    identificator(SchemeList, "/?#@:"),
    [:],
    { string_chars(Scheme, SchemeList) }.

identificator([H | T], List) -->
    [H],
    { valid_char(H, List) },
    identificator(T, List),
    !.
identificator([X | []], List) --> [X], { valid_char(X, List) }.

uri_userinfo(userinfo(UserInfo)) -->
    identificator(UserInfoList, "/?#@:"),
    [@],
    { string_chars(UserInfo, UserInfoList) },
    !.
uri_userinfo(userinfo([])) --> [].

uri_host_aux(host(Ip)) -->
    uri_ip(IpList),
    { string_chars(Ip, IpList) },
    !.
uri_host_aux(host(Host)) -->
    uri_host(HostList),
    { string_chars(Host, HostList) },
    !.

uri_host(X) -->
    identificator(A, "./?#@:"),
    [.],
    uri_host(B),
    {flatten([[A | [.]], B], X)},
    !.
uri_host(X) -->
    identificator(X, "./?#@:").

valid_char(X, List) :-
    char_type(X, ascii),
    not(char_type(X, space)),
    valid_char_aux(X, List).

valid_char_aux(_, []) :- !.
valid_char_aux(X, [Invalid_char | Rest]) :-
    X \= Invalid_char,
    valid_char_aux(X, Rest).

uri_port(port(Port)) -->
    [:],
    digits(PortList),
    {string_chars(Port, PortList)},
    !.
uri_port(port([])) --> [].

uri_fragment(fragment(Fragment)) -->
    uri_fragment_aux(FragmentList),
    {flatten(FragmentList, FlattenFragment)},
    {string_chars(Fragment, FlattenFragment)}.

uri_fragment_aux(FragmentList) -->
    [#],
    identificator(FragmentList, ""),
    !.
uri_fragment_aux([]) --> [].

uri_query(query(Query)) -->
    uri_query_aux(QueryList),
    {flatten(QueryList, FlattenQuery)},
    {string_chars(Query, FlattenQuery)}.

uri_query_aux(QueryList) -->
    [?],
    identificator(QueryList, "#"),
    !.
uri_query_aux([]) --> [].

uri_path(path(Path)) -->
    uri_path_aux(PathList),
    {flatten(PathList, FlattenPath)},
    {string_chars(Path, FlattenPath)}.

% path_aux//
% Parse the path starting with /
uri_path_aux(PathList) -->
    [/],
    identificator(A, "/?#@:"),
    uri_path_aux(B),
    {PathList = [[/ | A], B]},
    !.
uri_path_aux(PathList) -->
    [/],
    identificator(A, "/?#@:"),
    {PathList = [/ | A]},
    !.
% special method to parse the second type of URI
% scheme : path
% qui si rompe, Esempio:
% uri_parse("scheme://user@host:1ab23/path?queri#frag", X). 
% possibile soluzione: nome regola diversa per il secondo caso
uri_path_aux(PathList) -->
    identificator(A, "/?#@:"),
    uri_path_aux(B),
    {PathList = [[/ | A], B]},
    !.
uri_path_aux([]) --> [/], !.
uri_path_aux([]) --> [].
    
uri_ip(Ip) --> 
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

uri_default_port("http", "80").
uri_default_port("https", "443").
