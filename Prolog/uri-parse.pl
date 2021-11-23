% uri_parse/2

% news scheme
uri_parse(URIString, uri("news", _, Host, _)) :-
    string(URIString),
    string_chars(URIString, Chars),
    uri_schema(Chars, [n,e,w,s], RestAuthority),
    news_schema_host(RestAuthority, uri_auth(_, Host)),
    !.

% default
uri_parse(URIString, uri(Schema, UserInfo, Host, RestPath)) :-
    string(URIString),
    string_chars(URIString, Chars),
    uri_schema(Chars, SchemaList, RestAuthority),
    uri_authority(RestAuthority, uri_auth(UserInfo, Host), RestPath),
    string_chars(Schema, SchemaList),
    !.

% uri_schema/3

uri_schema([':' | Rest], [], Rest) :- !.
uri_schema([X | Xs], [X | Ys],  Rest) :-
    uri_schema(Xs, Ys, Rest).

% uri_authority/3

% authority type: userinfo@domain
uri_authority(['/', '/' | Host], ParsedAuth, Rest) :-
    parse_host(Host, ParsedHost, Rest),
    uri_userinfo(ParsedHost, ParsedInfo, Domain),
    ParsedAuth = uri_auth(ParsedInfo, Domain),
    !.

% authority type: domain
uri_authority(['/', '/' | Host], ParsedAuth, Rest) :-
    parse_host(Host, ParsedHost, Rest),
    ParsedAuth = uri_auth(_, ParsedHost),
    !.

parse_host([], [], _) :- !. % nel caso finisse qui URI
parse_host(['/' | Rest], [], Rest) :- !.
parse_host([X | Xs], [X | Ys],  Rest) :-
    invalid_char_host(X),
    parse_host(Xs, Ys, Rest).

% uri_userinfo/3
uri_userinfo(['@' | Rest], [], Rest) :- !.
uri_userinfo([X | Xs], [X | Ys],  Rest) :- 
    invalid_char(X),
    uri_userinfo(Xs, Ys, Rest).

% news_schema_host/
% authority type: userinfo@domain
news_schema_host(Host, ParsedAuth) :-
    parse_host(Host, ParsedHost, _),
    ParsedAuth = uri_auth(_, ParsedHost),
    !.

invalid_char('/') :- !, fail.
invalid_char('?') :- !, fail.
invalid_char('#') :- !, fail.
invalid_char('@') :- !, fail.
invalid_char(':') :- !, fail.
invalid_char(Char) :-
    char_type(Char, ascii).

invalid_char_host('.') :- !, fail.
invalid_char_host(X) :-
    invalid_char(X).