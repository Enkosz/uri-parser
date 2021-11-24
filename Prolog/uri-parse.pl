% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- set_prolog_flag(double_quotes, chars).

uri_parse(URIString, URI) :-
    phrase(uri(URI), URIString).

uri(X) -->
    scheme(A),
    authority(B),
    {X = [A, B]}.

authority(X) -->
    [/, /],
    userInfo(A),
    host_aux(B),
    port(C),
    {X = [A, B, C]}.

scheme(X) --> 
    identificator(X),
    [:].

identificator([H | T]) -->
    [H],
    { valid_char(H) },
    identificator(T),
    !.
identificator([X | []]) --> [X], { valid_char(X) }.

userInfo(X) -->
    identificator(X),
    [@],
    !.
userInfo([]) --> [].

host_aux(X) -->
    ip(X),
    !.
host_aux(X) -->
    host(X),
    !.

host(X) -->
    identificator(A),
    [.],
    host(B),
    {X = [A, B]},
    !.
host(X) -->
    identificator(X).

valid_char(X) :-
    char_type(X, alpha).

identificator_host([H | T]) -->
    [H],
    { valid_char(H), H \= '.'},
    identificator_host(T),
    !.
identificator_host([X | []]) --> [X], { valid_char(X), X \= '.'}.

port(X) -->
    [:],
    digits(X),
    !.
port([]) --> [].
    
ip(X) --> 
    triplets(A), ['.'], triplets(B), ['.'], triplets(C), ['.'], triplets(D), 
    { X = [A, '.', B, '.', C, '.', D] }.

triplets(X) --> 
    digit(A), digit(B), digit(C),
    { X = [A, B, C] }.

digit(X) --> [X], { is_digit(X) }.

digits([X | Xs]) -->
    digit(X),
    digits(Xs).
digits([X | []]) --> digit(X), !.