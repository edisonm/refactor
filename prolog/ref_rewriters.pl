:- module(ref_rewriters,
	  [expand_term/5,
	   expand_head/5,
	   expand_body/5,
	   expand_goal/5,
	   expand_sentence/4
	  ]).

:- use_module(library(ref_base)).

:- meta_predicate
	expand_term(+,+,-,0,+),
	expand_head(+,+,-,0,+),
	expand_body(+,+,-,0,+),
	expand_sentence(+,-,0,+),
	expand_goal(+,+,-,0,+).

%%	expand_term(?Sentence, ?Term, ?Replacement, :Expander, +Action)

expand_term(Sentence, Term, Into, Expander, Options) :-
    expand(term, Sentence, Term, Into, Expander, Options).

%%	expand_sentence(?Sentence, ?Into, :Expander, +Action).
%
% TODO:
% \footnote{Further versions will allow
%   \predref{expand\_sentence}{3} to return a list, as
%   \predref{term\_expansion}{2} does in many Prolog dialects.}

expand_head(Sentence, Term, Into, Expander, Options) :-
    expand(head, Sentence, Term, Into, Expander, Options).

expand_body(Sentence, Term, Into, Expander, Options) :-
    expand(body, Sentence, Term, Into, Expander, Options).

expand_sentence(M:Term, Into, Expander, Options) :-
    expand(sent, M:Term, Term, Into, Expander, Options).

expand_goal(Sentence, Goal, Into, Expander, Options) :-
    expand(goal, Sentence, Goal, Into, Expander, Options).
