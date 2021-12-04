%%%% -*- Mode: Prolog -*-

% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- module(uri_parse, [uri_parse/2]).

uri_parse(URIString, uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) :-
    string_chars(URIString, URIChars),
    uri_parse_(URIChars,
        uri(components(
            scheme(Scheme),
            userinfo(UserInfo), 
            host(Host),
            port(Port),
            path(Path), 
            query(Query), 
            fragment(Fragment)
        ))).

uri_parse_(URIString, uri(URI)) :-
    phrase(uri(URI), URIString).

% ["fax" | "tel"] ‘:’ userinfo 
uri(components(Scheme, UserInfo, host([]), port([]), path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    {current_scheme(Scheme)},
    !,
    uri_userinfo_scheme_syntax(UserInfo).

% "news" ‘:’ host 
uri(components(Scheme, userinfo([]), Host, port([]), path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    {Scheme = scheme('news')},
    !,
    uri_host_aux(Host).

% "mailto" ‘:’ userinfo ['@'' host] 
uri(components(Scheme, UserInfo, Host, port([]), path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    {Scheme = scheme('mailto')},
    !,
    uri_userinfo_scheme_syntax(UserInfo),
    [@],
    uri_host_aux(Host).

% "mailto" ‘:’ userinfo
uri(components(Scheme, UserInfo, host([]), port([]), path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    {Scheme = scheme('mailto')},
    !,
    uri_userinfo_scheme_syntax(UserInfo).

% TODO: "zos" ':' [userinfo '@'] host [: port] '/' path_zos [? query] [# fragment]
% suppundo che path sia obbligatorio, in caso non lo sia basta aggiungere un caso base vuoto
uri(components(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    {Scheme = scheme('zos')},
    !,
    [/, /],
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    uri_port(ActualPort),
    [/],
    uri_path_zos(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    {uri_default_port(Scheme, ActualPort, Port)},
    !.

% scheme ‘:’ authorithy[‘/’ [path] [‘?’ query] [‘#’ fragment]]
uri(components(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    [/, /],
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    uri_port(ActualPort),
    [/],
    uri_path(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    {uri_default_port(Scheme, ActualPort, Port)},
    !.

% scheme ‘:’ authorithy
uri(components(Scheme, UserInfo, Host, Port, path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    [/, /],
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    uri_port(ActualPort),
    {uri_default_port(Scheme, ActualPort, Port)},
    !.

% scheme ‘:’ [‘/’] [path] [‘?’ query] [‘#’ fragment]
uri(components(Scheme, userinfo([]), host([]), port([]), Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    ([/]; []),
    uri_path(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    !.

uri_scheme(scheme(Scheme)) --> 
    identificator(SchemeList, ['/', '?', '#', '@', ':', ' '], ascii),
    [:],
    { atom_chars(Scheme, SchemeList) }.

identificator(['%', '2', '0' | T], List, CharType) -->
    [' '],
    { valid_char(' ', List, CharType) },
    identificator(T, List, CharType),
    !.
identificator([H | T], List, CharType) -->
    [H],
    { valid_char(H, List, CharType) },
    identificator(T, List, CharType),
    !.
identificator([X | []], List, CharType) --> [X], { valid_char(X, List, CharType) }.

uri_userinfo(userinfo(UserInfo)) -->
    identificator(UserInfoList, ['/', '?', '#', '@', ':', ' '], ascii),
    [@],
    { atom_chars(UserInfo, UserInfoList) },
    !.

uri_userinfo(userinfo([])) --> [], !.

uri_userinfo_scheme_syntax(userinfo(UserInfo)) -->
    identificator(UserInfoList, ['/', '?', '#', '@', ':', ' '], ascii),
    { atom_chars(UserInfo, UserInfoList) },
    !.

uri_host_aux(host(Ip)) -->
    uri_ip(IpList),
    { atom_chars(Ip, IpList) },
    !.

uri_host_aux(host(Host)) -->
    uri_host(HostList),
    { atom_chars(Host, HostList) },
    !.

uri_ip(Ip) --> 
    triplets(A), [.], triplets(B), [.], triplets(C), [.], triplets(D), 
    { flatten([A, '.', B, '.', C, '.', D], Ip) }.

uri_host(X) -->
    identificator(A, ['.', '/', '?', '#', '@', ':', ' '], ascii),
    [.],
    uri_host(B),
    {flatten([[A | [.]], B], X)},
    !.

uri_host(X) -->
    identificator(X, ['.', '/', '?', '#', '@', ':', ' '], ascii).

uri_port(port(Port)) -->
    [:],
    digits(PortList),
    {atom_chars(Port, PortList)},
    !.
uri_port(port([])) --> [].

uri_fragment(fragment(Fragment)) -->
    uri_fragment_aux(FragmentList),
    {flatten(FragmentList, FlattenFragment)},
    {atom_chars(Fragment, FlattenFragment)}.

uri_fragment_aux(FragmentList) -->
    [#],
    identificator(FragmentList, [], ascii),
    !.

uri_fragment_aux([]) --> [].

uri_query(query(Query)) -->
    uri_query_aux(QueryList),
    {flatten(QueryList, FlattenQuery)},
    {atom_chars(Query, FlattenQuery)}.

uri_query(query([])) --> [], !.

uri_query_aux(QueryList) -->
    [?],
    identificator(QueryList, ['#'], ascii),
    !.

uri_path(path(Path)) -->
    uri_path_aux(PathList),
    !,
    {flatten(PathList, FlattenPath)},
    {atom_chars(Path, FlattenPath)}.

uri_path(path([])) --> [], !.

% path_aux//1
uri_path_aux(PathList) -->
    identificator(A, ['/', '?', '#', '@', ':'], ascii),
    [/],
    uri_path_aux(B),
    {PathList = [A, [/ | B]]},
    !.
uri_path_aux(PathList) -->
    identificator(PathList, ['/', '?', '#', '@', ':'], ascii),
    !.

uri_path_zos(path(Path)) -->
    uri_id44(Id44),
    ['('],
    uri_id8(Id8),
    [')'],
    {length(Id44, Id44Length), Id44Length =< 44},
    {length(Id8, Id8Length), Id8Length =< 8},
    {flatten([Id44, '(', Id8, ')'], FlattenPath)},
    {atom_chars(Path, FlattenPath)},
    !.

uri_path_zos(path(Path)) -->
    uri_id44(Id44),
    {length(Id44, Id44Length), Id44Length =< 44},
    {flatten(Id44, FlattenPath)},
    {atom_chars(Path, FlattenPath)},
    !.

uri_id44(Id44) -->
    identificator(A, ['.'], alnum),
    [.],
    uri_id44(B),
    {
        flatten([[A | [.]], B], Id44)
    },
    !.

uri_id44(Id44) -->
    identificator(Id44, ['.'], alnum),
    !.

uri_id8(Id8) -->
    identificator(Id8, [], alnum),
    !.

triplets(TripletsChars) --> 
    digit(A), digit(B), digit(C),
    {
        TripletsChars = [A, B, C],
        number_chars(TripletsNumber, TripletsChars),
        between(0, 255, TripletsNumber)
    }.

current_scheme(scheme('tel')) :- !.
current_scheme(scheme('fax')) :- !.

uri_default_port(scheme(http), port([]), port('80')) :- !.
uri_default_port(scheme(https), port([]), port('80')) :- !.
uri_default_port(_, ActualPort, ActualPort) :- !.

valid_char(X, List, CharType) :-
    char_type(X, CharType),
    valid_char_aux(X, List).

valid_char_aux(_, []) :- !.

valid_char_aux(X, [Invalid_char | Rest]) :-
    X \= Invalid_char,
    valid_char_aux(X, Rest).

digit(X) --> [X], { is_digit(X) }.

digits([X | Xs]) -->
    digit(X),
    digits(Xs).
digits([X | []]) --> digit(X), !.
    