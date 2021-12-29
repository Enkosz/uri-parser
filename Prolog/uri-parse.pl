%%%% -*- Mode: Prolog -*-

% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- module(uri_parse, [uri_parse/2, uri_display/1, uri_display/2]).

% uri_display
uri_display(URIStruct) :-
    current_output(CurrentStream),
    uri_display(URIStruct, CurrentStream).

uri_display(uri(Scheme, UserInfo, Host, Port, Path, Query, Frag), Stream) :-
    is_stream(Stream),
    format(Stream,
        'Scheme:      ~w~n\c
        UserInfo:    ~w~n\c
        Host:        ~w~n\c
        Port:        ~w~n\c
        Path:        ~w~n\c
        Query:       ~w~n\c
        Fragment:    ~w', [Scheme, UserInfo, Host, Port, Path, Query, Frag]),
    close(Stream),
    !.

uri_display(false, Stream) :-
    is_stream(Stream),
    format(Stream, 'Uri invalid'),
    !.
%------------------------------------------------------------------------------

% uri_parse
uri_parse(URIString, uri(Scheme, UserInfo, Host, Port, Path, Query, Frag)) :-
    string_chars(URIString, URIChars),
    uri_parse_(URIChars,
               uri(components(
		       scheme(Scheme),
		       userinfo(UserInfo), 
		       host(Host),
		       port(Port),
		       path(Path), 
		       query(Query), 
		       fragment(Frag)
		   ))).

uri_parse_(URIString, uri(URI)) :-
    phrase(uri(URI), URIString).

%------------------------------------------------------------------------------

% ["fax" | "tel"] ':' [userinfo] 
uri(components(Scheme, UserInfo, host([]), port(80),
	       path([]), query([]), fragment([]))) -->
    uri_scheme(Scheme),
    {current_scheme(Scheme)},
    !,
    uri_userinfo_scheme_syntax(UserInfo).



%------------------------------------------------------------------------------

% "news" ':' [host] 
uri(components(scheme(Scheme),
	       userinfo([]), Host, port(80),
	       path([]), query([]), fragment([]))) -->
    uri_scheme(scheme(Scheme)),
    {scheme_equals_ignore_case(Scheme, "news")},
    uri_host_aux(Host),
    !.

uri(components(scheme(Scheme),
	       userinfo([]), host([]), port(80),
	       path([]), query([]), fragment([]))) -->
    uri_scheme(scheme(Scheme)),
    {scheme_equals_ignore_case(Scheme, "news")},
    !.

%------------------------------------------------------------------------------

% "mailto" ':' [userinfo ['@'' host]]
uri(components(scheme(Scheme), UserInfo, Host, port(80),
	       path([]), query([]), fragment([]))) -->
    uri_scheme(scheme(Scheme)),
    {scheme_equals_ignore_case(Scheme, "mailto")},
    uri_userinfo_scheme_syntax(UserInfo),
    [@],
    uri_host_aux(Host), 
    !.

% "mailto" ':' [userinfo]
uri(components(scheme(Scheme), UserInfo, host([]), port(80),
	       path([]), query([]), fragment([]))) -->
    uri_scheme(scheme(Scheme)),
    {scheme_equals_ignore_case(Scheme, "mailto")},
    uri_userinfo_scheme_syntax(UserInfo),
    !.

uri(components(scheme(Scheme), userInfo([]), host([]), port(80),
	       path([]), query([]), fragment([]))) -->
           uri_scheme(scheme(Scheme)), 
           {scheme_equals_ignore_case(Scheme, "mailto")},
           !.

%------------------------------------------------------------------------------

% zos
uri(components(scheme(Scheme), UserInfo, Host, Port, Path, Query, Fragment)) -->
    uri_scheme(scheme(Scheme)),
    {scheme_equals_ignore_case(Scheme, "zos")},
    !,
    uri_authority(components(UserInfo, Host, Port)),
    uri_subdomain(components(Path, Query, Fragment)),
    {checkPath(Path)}.


%------------------------------------------------------------------------------

% URI
uri(components(Scheme, UserInfo, Host, Port, Path, Query, Fragment)) -->
    uri_scheme(Scheme),
    uri_authority(components(UserInfo, Host, Port)),
    uri_subdomain(components(Path, Query, Fragment)),
    !.

%------------------------------------------------------------------------------

% Uri authority
uri_authority(components(UserInfo, Host, Port)) -->
    [/, /],
    uri_userinfo(UserInfo),
    uri_host_aux(Host),
    uri_port(ActualPort),
    {uri_default_port(ActualPort, Port)},
    !.

uri_authority(components(userinfo([]), host([]), port(80))) -->
    [], !.

%------------------------------------------------------------------------------

uri_subdomain(components(Path, Query, Fragment)) -->
    [/],
    uri_path(Path),
    uri_query(Query),
    uri_fragment(Fragment),
    !.

uri_subdomain(components(path([]), query([]), fragment([]))) -->
    [], !.

%------------------------------------------------------------------------------

% scheme
uri_scheme(scheme(Scheme)) --> 
    identificator(SchemeList, ['/', '?', '#', '@', ':', ' '], ascii),
    [:],
    { atom_chars(Scheme, SchemeList) }.

current_scheme(scheme(Scheme)) :- 
    scheme_equals_ignore_case(Scheme, "tel"), 
    !.
    
current_scheme(scheme(Scheme)) :- 
    scheme_equals_ignore_case(Scheme, "fax"), 
    !.

%------------------------------------------------------------------------------

% userinfo
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

uri_userinfo_scheme_syntax(userinfo([])) -->
    [], !.

%------------------------------------------------------------------------------

% host
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

%------------------------------------------------------------------------------

% port
uri_port(port(Port)) -->
    [:],
    digits(PortList),
    {number_chars(Port, PortList)},
    !.
uri_port(port([])) --> [].

uri_default_port(port([]), port(80)) :- !.
uri_default_port(ActualPort, ActualPort) :- 
    ActualPort \= port([]), !.

%------------------------------------------------------------------------------

%path
uri_path(path(Path)) -->
    uri_path_aux(PathList),
    !,
    {flatten(PathList, FlattenPath),
     atom_chars(Path, FlattenPath)}.

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

uri_path_zos -->
    uri_id44(Id44),
    ['('],
    uri_id8(Id8),
    [')'],
    {[C | _] = Id44,
     char_type(C, alpha),
     [Ch | _] = Id8,
     char_type(Ch, alpha),
     length(Id44, Id44Length), Id44Length =< 44,
     length(Id8, Id8Length), Id8Length =< 8},
    !.

uri_path_zos -->
    uri_id44(Id44),
    {[C | _] = Id44,
     char_type(C, alpha),
     length(Id44, Id44Length), Id44Length =< 44},
    !.

uri_id44(Id44) -->
    identificator(A, ['.', ' '], alnum),
    [.],
    uri_id44(B),
    {flatten([[A | [.]], B], Id44)},
    !.

uri_id44(Id44) -->
    identificator(Id44, ['.', ' '], alnum),
    !.

uri_id44(['.'| T]) -->
    ['.'],
    uri_id44(T),
    !.

uri_id8(Id8) -->
    identificator(Id8, [' '], alnum),
    !.

checkPath(path(AtomPath)) :-
    atom(AtomPath),
    atom_chars(AtomPath, CharsPath),
    phrase(uri_path_zos,  CharsPath),
    !.

%------------------------------------------------------------------------------

% query
uri_query(query(Query)) -->
    uri_query_aux(QueryList),
    {flatten(QueryList, FlattenQuery),
     atom_chars(Query, FlattenQuery)},
    !.

uri_query(query([])) --> [], !.

uri_query_aux(QueryList) -->
    [?],
    identificator(QueryList, ['#'], ascii),
    !.

%------------------------------------------------------------------------------

% fragment
uri_fragment(fragment(Fragment)) -->
    uri_fragment_aux(FragmentList),
    !,
    {flatten(FragmentList, FlattenFragment),
     atom_chars(Fragment, FlattenFragment)}.

uri_fragment(fragment([])) --> [], !.

uri_fragment_aux(FragmentList) -->
    [#],
    identificator(FragmentList, [], ascii),
    !.

%------------------------------------------------------------------------------

% identificator
identificator(['%', '2', '0' | T], List, CharType) -->
    [' '],
    {valid_char(' ', List, CharType)},
    identificator(T, List, CharType),
    !.
identificator([H | T], List, CharType) -->
    [H],
    {valid_char(H, List, CharType)},
    identificator(T, List, CharType),
    !.
identificator([X | []], List, CharType) --> 
    [X], 
    {valid_char(X, List, CharType)}.

%------------------------------------------------------------------------------

% valid char
valid_char(X, List, CharType) :-
    char_type(X, CharType),
    valid_char_aux(X, List).

valid_char_aux(_, []) :- !.

valid_char_aux(X, [Invalid_char | Rest]) :-
    X \= Invalid_char,
    valid_char_aux(X, Rest).

%------------------------------------------------------------------------------

triplets(TripletsChars) --> 
    digit(A), digit(B), digit(C),
    {TripletsChars = [A, B, C],
     number_chars(TripletsNumber, TripletsChars),
     between(0, 255, TripletsNumber) }.

digit(X) --> [X], { is_digit(X) }.

digits([X | Xs]) -->
    digit(X),
    digits(Xs).
digits([X | []]) --> digit(X), !.

%------------------------------------------------------------------------------

scheme_equals_ignore_case(Atom, Scheme) :-
        atom_string(Atom, String),
        string_lower(String, StringLowerCase),
        StringLowerCase = Scheme.
