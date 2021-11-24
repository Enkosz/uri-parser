% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- set_prolog_flag(double_quotes, chars).

uri_parse_(URIString, uri(Scheme, Authority, Path)) :-
    phrase(uri(uristructure(Scheme, Authority, Path)), URIString).

uri(uristructure(Scheme, Authority, Path)) -->
    scheme(Scheme),
    authority(Authority),
    path(Path).

authority(uri_authority(UserInfo, Host, Port)) -->
    [/, /],
    user_info(UserInfo),
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

user_info(user_info(UserInfo)) -->
    identificator(UserInfoList),
    [@],
    { string_chars(UserInfo, UserInfoList) },
    !.
user_info(user_info([])) --> [].

host_aux(host(Ip)) -->
    ip(IpList),
    { string_chars(Ip, IpList) },
    !.
host_aux(host(Host)) -->
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

port(port(Port)) -->
    [:],
    digits(PortList),
    {string_chars(Port, PortList)},
    !.
port(port([])) --> [].

path(path(Path)) -->
    [/],
    identificator(A),
    path(B),
    {flatten([A, [B | [/]]], Path)},
    !.
path(path(Path)) -->
    identificator(PathList),
    {string_chars(Path, PathList)}.
path([]) --> [].
    
ip(Ip) --> 
    triplets(A), ['.'], triplets(B), ['.'], triplets(C), ['.'], triplets(D), 
    { flatten([A, '.', B, '.', C, '.', D], Ip) }.

triplets(X) --> 
    digit(A), digit(B), digit(C),
    { X = [A, B, C] }.

digit(X) --> [X], { is_digit(X) }.

digits([X | Xs]) -->
    digit(X),
    digits(Xs).
digits([X | []]) --> digit(X), !.