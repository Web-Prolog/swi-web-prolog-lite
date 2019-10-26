:- module(rpc, [
      rpc/2, 
      rpc/3
   ]).

:- use_module(library(http/http_open)).


rpc(URI, Query) :-
    rpc(URI, Query, []).
    
rpc(URI, Query, Options) :-
    parse_url(URI, Parts),
    option(limit(Limit), Options, 1),
    format(atom(QueryAtom), "(~p)", [Query]),
    rpc(Query, 0, Limit, QueryAtom, Parts, Options).
    
rpc(Query, Offset, Limit, QueryAtom, Parts, Options) :-    
    parse_url(ExpandedURI, [
        path('/ask'),
        search([query=QueryAtom, offset=Offset, limit=Limit, format=prolog])
      | Parts
    ]),
    setup_call_cleanup(
        http_open(ExpandedURI, Stream, []),
        read(Stream, Answer), 
        close(Stream)),
    wait_answer(Answer, Query, Offset, Limit, QueryAtom, Parts, Options).

wait_answer(error(Error), _, _, _, _, _, _) :-
    throw(Error).
wait_answer(failure, _, _, _, _, _, _) :-
    fail.
wait_answer(success(Solutions, false), Query, _, _, _, _, _) :- 
    !,
    member(Query, Solutions).
wait_answer(success(Solutions, true), Query, Offset0, Limit, QueryAtom, Parts, Options) :-
    (   member(Query, Solutions)
    ;   Offset is Offset0 + Limit,
        rpc(Query, Offset, Limit, QueryAtom, Parts, Options)
    ).
