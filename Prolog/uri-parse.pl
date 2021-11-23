% uri_parse/2

uri_parse(URIString, uri(Schema, UserInfo, Authority, RestPath)) :-
    string(URIString),
    string_chars(URIString, Chars),
    uri_schema(Chars, SchemaList, ['/', '/' | RestUserInfo]),
    uri_userinfo(RestUserInfo, UserInfoList, RestAuthority),
    uri_authority(RestAuthority, AuthorityList, RestPath),
    string_chars(Schema, SchemaList),
    string_chars(Authority, AuthorityList),
    string_chars(UserInfo, UserInfoList).

% uri_schema/3

uri_schema([':' | Rest], [], Rest) :- !.
uri_schema([X | Xs], [X | Ys],  Rest) :-
    uri_schema(Xs, Ys, Rest).

% uri_authority/3

uri_authority(Host, ParsedHost, Rest) :-
    uri_host(Host, ParsedHost, Rest).

uri_host([], [], _) :- !. % nel caso finisse qui URI
uri_host(['/' | Rest], [], Rest) :- !.
uri_host([X | Xs], [X | Ys],  Rest) :- 
    uri_host(Xs, Ys, Rest).

% uri_userinfo/3
uri_userinfo(['@' | Rest], [], Rest) :- !.
uri_userinfo([X | Xs], [X | Ys],  Rest) :- 
    uri_userinfo(Xs, Ys, Rest).