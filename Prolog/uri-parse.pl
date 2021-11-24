% 866163 Avallone Lorenzo
% 872783 Giannino Simone
% 866147 Biotto Simone

:- set_prolog_flag(double_quotes, chars).

scheme(X) --> 
    identificator(X),
    [:].

identificator([H | T]) -->
    [H],
    { valid_char(H) },
    identificator(T),
    !.
identificator([X | []]) --> [X], { valid_char(X) }.

valid_char(X) :-
    char_type(X, ascii).
    X 

ip(X) --> 
    triplets(A), ['.'], triplets(B), ['.'], triplets(C), ['.'], triplets(D), 
    { X = [A, '.', B, '.', C, '.', D | []] }.
triplets(X) --> 
    digit(A), digit(B), digit(C),{ X = [A,B,C] }.

digit(X) --> [X], { is_digit(X) }.