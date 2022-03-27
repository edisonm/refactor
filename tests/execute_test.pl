:- module(refactor_execute_test,
          [do_execute_test/3,
           execute_test/2,
           execute_test/3,
           execute_test/4,
           gen_random_pred/3
          ]).

:- use_module(library(option)).
:- use_module(library(random)).
:- use_module(library(comment_data)).
:- use_module(library(call_in_dir)).
:- use_module(library(refactor)).
:- use_module(library(mapargs)).

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

generate_arg(Id, Desc, Value) :- generate_arg_(Desc, Id, Value).

generate_arg_(id, Id, Id).
generate_arg_(randnum, _, Arg) :- random(Arg).
generate_arg_(randatm, _, Arg) :- random(Val), atom_number(Arg, Val).
generate_arg_(randstr, _, Arg) :- random(Val), number_string(Val, Arg).

gen_random_pred(Desc, N, Options) :-
    option(file(Alias), Options, plbin(random)),
    absolute_file_name(Alias, File, [file_type(prolog)]),
    functor(Desc, F, A),
    functor(Fact, F, A),
    tell(File),
    forall(between(1, N, Id),
           ( mapargs(generate_arg(Id), Desc, Fact),
             portray_clause(Fact)
           )),
    told.
