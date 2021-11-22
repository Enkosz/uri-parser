% uri_parse/2

uri_parse(URIString, uri(Schema, Authority, Rest)) :-
    string(URIString),
    string_chars(URIString, Chars),
    uri_schema(Chars, SchemaList, Rest),
    uri_authority(Rest, Authority, Rest),
    string_chars(Schema, SchemaList).

uri_schema([':' | Rest], [], Rest) :- !.
uri_schema([X | Xs], [X | Ys],  Rest) :-
    uri_schema(Xs, Ys, Rest).

uri_authority(['/', '/' | Host], ParsedHost, Rest) :-
    uri_host(Host, ParsedHost, Rest).

uri_host([], [], []) :- !.
uri_host(['/' | L], [], ['/' | L]) :- !.