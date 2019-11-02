:- encoding(utf8).
/*  Author:        Torbjörn Lager
    E-mail:        torbjorn.lager@gu.se
    Copyright (C): 2019, Torbjörn Lager,

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(node, [nodeinfo/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(sandbox)).

:- use_module(rpc).
:- use_module(src).

:- use_module(src:rpc).


:- use_module(library(debug)).

:- dynamic cache/3. % QueryID, Index, Pid

%:- debug(cache).
%:- debug(err).

:- http_handler(/,         http_reply_file('shell.html', []), []         ).
:- http_handler(root(src), http_reply_file('src.pl',     []), []         ).
:- http_handler(root(ask), http_ask,                          [spawn([])]).


:- setting(cache_maxsize, integer, 100, 'Max number of cache entries').
:- setting(timeout,       number,  1,   'Timeout in seconds').


http_ask(Request) :-
    http_parameters(Request,
        [ query(GoalAtom, []),
          offset(Offset, [integer, default(0)]),
          limit(Limit,   [integer, default(1)]),
          format(Format, [atom, default(json)])
        ]),
    catch(read_term_from_atom(GoalAtom, Goal, [variable_names(Bindings)]), Error, true),
    (    var(Error)
    ->   fix_template(Format, Goal, Bindings, NewTemplate), 
         find_answer(NewTemplate, Goal, Offset, Limit, Answer), 
         output_result(Format, Answer)
    ;    output_result(Format, error(Error))
    ).


%!  fix_template(+Format, +Template, +Bindings, -NewTemplate) is det.
%
%   Generate the template for json(-s) Format  from the variables in
%   the asked Goal. Variables starting  with an underscore, followed
%   by an capital letter are ignored from the template. If a json
%   format is specified, the template option is ignored.

fix_template(Format, _Template, Bindings, NewTemplate) :-
    json_lang(Format),
    !,    
    exclude(anon, Bindings, NamedBindings),
    dict_create(NewTemplate, json, NamedBindings).
fix_template(_, Template, _, Template).


anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).


%!  json_lang(+Format) is semidet.
%
%   True if Format is a JSON variation.

json_lang(json) :- !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').
        
    
%!  output_result(+Format, +EventTerm) is det.
%!  output_result(+Format, +EventTerm, +OptionsDict) is det.
%
%   Formulate an HTTP response from a pengine event term. Format is
%   one of =prolog=, =json= or =json-s=.

output_result(prolog, Answer) :-
    !,
    format('Content-type: text/plain; charset=UTF-8~n~n'),
    write_term(Answer, [ 
        quoted(true),
        ignore_ops(true),
        fullstop(true),
        nl(true),
        blobs(portray),
        portray_goal(portray_blob)
    ]).
output_result(json, Answer) :-
    answer_to_json(Answer, JSON),
    reply_json(JSON).
    

answer_to_json(success(Bindings0, More), json{type:success, data:Bindings, more:More}) :-
    maplist(bindings_to_json_strings, Bindings0, Bindings).
answer_to_json(error(ErrorTerm), json{type:error, data:ErrorString}) :-
    message_to_string(ErrorTerm, ErrorString).
answer_to_json(failure, json{type:failure}).



%!  answer_to_json_strings(+Pengine, +AnswerDictIn, -AnswerDict).
%
%   Translate answer dict with Prolog term values into answer dict
%   with string values.

bindings_to_json_strings(DictIn, DictOut) :-
    dict_pairs(DictIn, Tag, Pairs),
    maplist(term_string_value, Pairs, BindingsOut),
    dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(N-V, N-A) :-
    with_output_to(string(A), write_term(V,[ quoted(true)])).




%!  find_answer(+Template, +Query, +Offset, +Limit, -Answer) is det.


find_answer(Template, Query, Offset, Limit, Answer) :-
    debug(cache, 'Query: ~p', [Query]),
    thread_self(Self),
    query_id(Query, QueryID),
    (   with_mutex(cache, retract(cache(QueryID, Offset, Pid)))
    ->  next(Pid, Self, Limit)
    ;   ask(Template, Query, Offset, Limit, Pid)
    ),
    wait_for_answer(Self, Pid, QueryID, Offset, Limit, Answer),
    get_flag(cache_size, Size),
    (   Size > MaxSize
    ->  setting(cache_remprop, RemProp),
        Mtop is integer(MaxSize*RemProp),
        once(findnsols(Mtop, Pid0, with_mutex(cache, cache(_, _, Pid0)), Pids)),
        with_mutex(cache, maplist(kill_thread, Pids)) 
    ;   true
    ).

query_id(Term, QueryID) :-
    copy_term(Term, QueryID0),
    numbervars(QueryID0, 0, _),
    term_hash(QueryID0, QueryID).
    


ask(Template, Query, Offset, Limit, Pid) :-
    uuid(Pid),
    thread_self(Self),
    thread_create(query(Template, Query, Offset, Limit, Self), _Pid, [
	    alias(Pid),
        at_exit(done(Pid))
    ]),
    flag(cache_size, N, N+1).
	

:- endif.
    
    
query(Template, Query, Offset, Limit, Parent) :-
    catch(guarded_query(Template, Query, Offset, Limit, Parent), Error, 
          thread_send_message(Parent, error(Error))).
    
guarded_query(Template, Query, Offset, Limit, Parent) :-
    State = count(Limit),
    Target = target(Parent),
    (   call_cleanup(solve(Template, Query, Offset, State, Solutions), Det=true),
        arg(1, Target, Parent2),
        (   var(Det)
        ->  thread_send_message(Parent2, success(Solutions, true)),
            thread_get_message(next(Parent3, Count)),
            nb_setarg(1, State, Count),
            nb_setarg(1, Target, Parent3),
            fail
        ;   thread_send_message(Parent2, success(Solutions, false))
        )
    ;   arg(1, Target, Parent2),
        thread_send_message(Parent2, failure)
    ).
    

solve(Template, Query, Offset, State, Solutions) :-
    safe_goal(Query),
    findnsols(State, Template, offset(Offset, src:Query), Solutions),
    Solutions \== [].
    

done :-
    retractall(cache(_, _, Me)),
    debug(cache, 'DONE: ~p', [Me]),
    thread_detach(Me),
    flag(cache_size, N, N-1).


next(Pid, Parent, Limit) :-
    thread_send_message(Pid, next(Parent, Limit)).



wait_for_answer(Self, Pid, QueryID, Offset, Limit, Answer) :-
    setting(timeout, Timeout),
    (   thread_get_message(Self, Answer, [timeout(Timeout)])
    ->  (   Answer = success(_, true)
        ->  Index is Offset + Limit,
            assertz(cache(QueryID, Index, Pid)),
        ;   true
        )
    ;   Answer = error(error(timeout_exceeded, Timeout)),
        kill_thread(Pid)
    ).

print_cache :-
    listing(cache/3), 
    get_flag(cache_size, S), 
    format('Cache size: ~p', [S]).


kill_thread(Pid) :-
    % Use catch/3 since Pid may be dead already
    catch(thread_detach(Pid), _, true),
    catch(thread_signal(Pid, thread_exit(Pid)), _, true).
    
        
    
:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(rpc:rpc(_,_,_)).
sandbox:safe_primitive(system:get_flag(_,_)).


:- multifile
    prolog:message//1.


prolog:message(error(timeout_exceeded, Timeout)) -->
    [ 'Error: Time limit (~ps) exceeded.'-[Timeout]].


nodeinfo([ 
      profile(isobase),
      timeout(Timeout),
      cache_maxsize(MaxSize),
      cache_cursize(Size)
  ]) :-
    setting(timeout, Timeout),    
    setting(cache_maxsize, MaxSize),
    get_flag(cache_size, Size).
    
    
server(Host, Port) :-    
    http_server(http_dispatch, [
         port(Host:Port),
         workers(24)
    ]).

:- server('localhost',3010).
