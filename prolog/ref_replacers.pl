:- module(ref_replacers,
          [replace_term/3,
           replace_term/4,
           replace_head/3,
           replace_head/4,
           replace_body/3,
           replace_body/4,
           replace_goal/3,
           replace_goal/4,
           replace_sentence/3,
           replace_sentence/4,
           replace/4
          ]).

:- use_module(library(ref_replace)).

:- meta_predicate
    replace_term(+,-,0,:),
    replace_head(+,-,0,:),
    replace_body(+,-,0,:),
    replace_sentence(+,-,0,:),
    replace_term(+,-,:),
    replace_head(+,-,:),
    replace_body(+,-,:),
    replace_sentence(+,-,:),
    replace_goal(+,-,0,:),
    replace(+,+,-,:).

%%  replace_goal(?Goal, ?Into, :Expander, +Options)
%
replace_goal(Goal, Into, Expander, Options) :-
    replace(goal, Goal, Into, Expander, Options).

%%  replace_term(?Term, ?Into, :Expander, +Options)
%
replace_term(Term, Into, Expander, Options) :-
    replace(term, Term, Into, Expander, Options).

%%  replace_head(?Term, ?Into, :Expander, +Options)
%
replace_head(Term, Into, Expander, Options) :-
    replace(head, Term, Into, Expander, Options).

%%  replace_body(?Term, ?Into, :Expander, +Options)
%
replace_body(Term, Into, Expander, Options) :-
    replace(body, Term, Into, Expander, Options).

replace_term(Term, Into, Options) :-
    replace_term(Term, Into, true, Options).

replace_head(Term, Into, Options) :-
    replace_head(Term, Into, true, Options).

replace_body(Term, Into, Options) :-
    replace_body(Term, Into, true, Options).

replace_sentence(Sentence, Into, Options) :-
    replace_sentence(Sentence, Into, true, Options).

:- meta_predicate replace_goal(?,?,+).
replace_goal(Term, Into, Options) :-
    replace_goal(Term, Into, true, Options).

%%  replace_sentence(?Sentence, ?Into, :Expander, +Options).
%
replace_sentence(Sentence, Into, Expander, Options) :-
    replace(sent, Sentence, Into, Expander, Options).

%%  replace(+Scope, ?Term, ?Into, :Expander, +Options).
%
replace(Scope, Term, Into, Options) :-
    replace(Scope, Term, Into, true, Options).
