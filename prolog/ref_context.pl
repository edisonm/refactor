/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
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

:- module(ref_context,
          [refactor_context/2
          ]).

%%      refactor_context(?Name, ?Value) is nondet.

refactor_context(variable_names, VarNames) :-
    b_getval(refactor_variable_names, VarNames).
refactor_context(pattern, Pattern) :-
    b_getval(refactor_pattern, Pattern).
refactor_context(into, Into) :-
    b_getval(refactor_into, Into).
refactor_context(sentence, Sentence) :-
    b_getval(refactor_sentence, Sentence).
refactor_context(sent_pattern, SentPattern) :-
    b_getval(refactor_sent_pattern, SentPattern).
refactor_context(options, Options) :-
    b_getval(refactor_options, Options).
refactor_context(goal_args, Sentence) :-
    b_getval(refactor_goal_args, Sentence).
refactor_context(termpos, TermPos) :-
    b_getval(refactor_termpos, TermPos).
refactor_context(file, File) :-
    b_getval(refactor_file, File).
refactor_context(pos, File) :-
    b_getval(refactor_pos, File).
refactor_context(preffix, Px) :-
    b_getval(refactor_preffix, Px).
refactor_context(text, Text) :-
    b_getval(refactor_text, Text).
