/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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
    replace_term(?,?,0,:),
    replace_head(?,?,0,:),
    replace_body(?,?,0,:),
    replace_sentence(?,?,0,:),
    replace_term(?,?,:),
    replace_head(?,?,:),
    replace_body(?,?,:),
    replace_sentence(?,?,:),
    replace_goal(?,?,0,:),
    replace_goal(?,?,:),
    replace(+,?,?,:).

%!  replace_goal(?Goal, ?Into, :Expander, +Options)
%
replace_goal(Goal, Into, Expander, Options) :-
    replace(goal, Goal, Into, Expander, Options).

%!  replace_term(?Term, ?Into, :Expander, +Options)
%
replace_term(Term, Into, Expander, Options) :-
    replace(term, Term, Into, Expander, Options).

%!  replace_head(?Term, ?Into, :Expander, +Options)
%
replace_head(Term, Into, Expander, Options) :-
    replace(head, Term, Into, Expander, Options).

%!  replace_body(?Term, ?Into, :Expander, +Options)
%
replace_body(Term, Into, Expander, Options) :-
    replace(body, Term, Into, Expander, Options).

%!  replace_term(?Term, ?Into, +Options)
%
replace_term(Term, Into, Options) :-
    replace_term(Term, Into, true, Options).

%!  replace_head(?Term, ?Into, +Options)
%
replace_head(Term, Into, Options) :-
    replace_head(Term, Into, true, Options).

%!  replace_body(?Term, ?Into, +Options)
%
replace_body(Term, Into, Options) :-
    replace_body(Term, Into, true, Options).

%!  replace_sentence(?Term, ?Into, +Options)
%
replace_sentence(Sentence, Into, Options) :-
    replace_sentence(Sentence, Into, true, Options).

%!  replace_goal(?Goal, ?Into, +Options)
%
replace_goal(Term, Into, Options) :-
    replace_goal(Term, Into, true, Options).

%!  replace_sentence(?Sentence, ?Into, :Expander, +Options).
%
replace_sentence(Sentence, Into, Expander, Options) :-
    replace(sent, Sentence, Into, Expander, Options).

%!  replace(+Scope, ?Term, ?Into, +Options).
%
replace(Scope, Term, Into, Options) :-
    replace(Scope, Term, Into, true, Options).
