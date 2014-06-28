:- module(ref_expanders,
	  [expand_term/5,
	   expand_head/5,
	   expand_body/5,
	   expand_goal/5,
	   expand_sentence/4
	  ]).

:- use_module(library(ref_expand)).

:- meta_predicate
	expand_term(+,+,-,0,+),
	expand_head(+,+,-,0,+),
	expand_body(+,+,-,0,+),
	expand_sentence(+,-,0,+),
	expand_goal(+,+,-,0,+).

%%	expand_goal(?Sentence, ?Term, ?Into, :Expander, +Action)
%
expand_goal(Sentence, Goal, Into, Expander, Options) :-
    expand(goal, Sentence, Goal, Into, Expander, Options).

%%	expand_term(?Sentence, ?Term, ?Into, :Expander, +Action)
%
expand_term(Sentence, Term, Into, Expander, Options) :-
    expand(term, Sentence, Term, Into, Expander, Options).

%%	expand_head(?Sentence, ?Term, ?Into, :Expander, +Action)
%
expand_head(Sentence, Term, Into, Expander, Options) :-
    expand(head, Sentence, Term, Into, Expander, Options).

%%	expand_body(?Sentence, ?Term, ?Into, :Expander, +Action)
%
expand_body(Sentence, Term, Into, Expander, Options) :-
    expand(body, Sentence, Term, Into, Expander, Options).

%%	expand_sentence(?Sentence, ?Into, :Expander, +Action).
%
expand_sentence(Term, Into, Expander, Options) :-
    expand(sent, Term, Term, Into, Expander, Options).
