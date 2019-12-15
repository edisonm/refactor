:- module(refactor_execute_test,
          [execute_test/2,
           execute_test/3,
           do_execute_test/3,
           execute_test/4]).

:- use_module(library(comment_data)).
:- use_module(library(call_in_dir)).
:- use_module(library(refactor)).

:- meta_predicate
    execute_test(+,1),
    execute_test(+,1,+),
    execute_test(+,+,1,+).

execute_test(Module, Goal) :-
    execute_test(Module, Module, Goal, []).

execute_test(Module, Test, Goal, Options) :-
    execute_test(Test, Goal, [module(Module)|Options]).

execute_test(Test, Goal, Options) :-
    rreset,
    do_execute_test(Test, Goal, Options).

do_execute_test(Test, Goal, Options) :-
    call_in_module_dir(refactor_execute_test,
                       ( call(Goal, Options),
                         with_output_to(string(Result), rshow)
                       )),
    comment_data(Test, Pattern),
    ( Pattern \== Result
    ->format("~s", [Result])
    ; true
    ),
    assertion(Pattern == Result).

