:- module(ref_expanders,
	  [expand_term/4,
	   expand_head/4,
	   expand_body/4,
	   expand_goal/4,
	   expand_sentence/4
	  ]).

:- use_module(library(ref_expand)).

:- meta_predicate
	expand_term(+,-,0,+),
	expand_head(+,-,0,+),
	expand_body(+,-,0,+),
	expand_sentence(+,-,0,+),
	expand_goal(+,-,0,+).

%%	expand_goal(?Goal, ?Into, :Expander, +Action)
%
expand_goal(Goal, Into, Expander, Options) :-
    expand(goal, Goal, Into, Expander, Options).

%%	expand_term(?Term, ?Into, :Expander, +Action)
%
expand_term(Term, Into, Expander, Options) :-
    expand(term, Term, Into, Expander, Options).

%%	expand_head(?Term, ?Into, :Expander, +Action)
%
expand_head(Term, Into, Expander, Options) :-
    expand(head, Term, Into, Expander, Options).

%%	expand_body(?Term, ?Into, :Expander, +Action)
%
expand_body(Term, Into, Expander, Options) :-
    expand(body, Term, Into, Expander, Options).

%%	expand_sentence(?Sentence, ?Into, :Expander, +Action).
%
expand_sentence(Sentence, Into, Expander, Options) :-
    expand(sent, Sentence, Into, Expander, Options).
