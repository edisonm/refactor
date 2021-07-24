/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(ref_context,
          [refactor_context/2,
           set_refactor_context/2,
           nb_set_refactor_context/2,
           with_from/2,
           with_refactor_context/3,
           with_termpos/2,
           with_varnames/2
          ]).

:- use_module(library(context_values)).

:- meta_predicate
    refactor_context(?, ?),
    set_refactor_context(?, ?),
    nb_set_refactor_context(?, ?),
    with_from(0, ?),
    with_termpos(0, ?),
    with_varnames(0, ?),
    with_refactor_context(0, ?, ?).

%!  refactor_context(?Name, ?Value) is nondet.

refactor_context(Name, Value) :-
    current_context_value(Name, Value).

set_refactor_context(Name, Value) :-
    set_context_value(Name, Value).

nb_set_refactor_context(Name, Value) :-
    nb_set_context_value(Name, Value).

with_termpos(Goal, TermPos) :-
    with_context_values(Goal, [termpos], [TermPos]).

with_varnames(Goal, VNL) :-
    with_context_values(Goal, [variable_names], [VNL]).

with_from(Goal, From) :-
    with_context_values(Goal, [from], [From]).

with_refactor_context(Goal, Names, Values) :-
    with_context_values(Goal, Names, Values).
