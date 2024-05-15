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

:- module(seek_text,
          [ comment_bound/2,
            comment_bound/3,
            seek1_char_left/4,
            seekn_char_right/6,
            seek_sub_string/6
          ]).

:- use_module(library(lists)).
:- use_module(library(ref_context)).

comment_bound(CommentL, From, To) :-
    member(Pos-Text, CommentL),
    stream_position_data(char_count, Pos, From),
    string_length(Text, Length),
    nl_mark_end(Text, Delta),
    To is Length + Delta + From.

nl_mark_end(_, 0 ).

/*
nl_mark_end(Text, Delta) :-
    ( string_code(1, Text, 0'%)
    ->Delta = 1
    ; Delta = 0
    ).
*/

comment_bound(From, To) :-
    refactor_context(comments, CommentL),
    comment_bound(CommentL, From, To).

comment_bound(From1, To1, From, To) :-
    comment_bound(FromC, ToC),
    ( FromC < To1
    ->true
    ; FromC = To1
    ->!
    ; !,
      fail
    ),
    ( ToC < To1
    ->To = ToC
    ; !,
      To = To1
    ),
    ( From1 =< FromC
    ->From = FromC
    ; From1 =< ToC
    ->From = From1
    ).

seek_sub_string(Text, SubText, SubTextN, F, T1, T) :-
    S = s(T1),
    ( comment_bound(T1, F, FC, TC)
    ; FC = F,
      TC = F
    ),
    arg(1, S, T2),
    ( D1 is FC - T2,
      D1 > 0,
      sub_string(Text, T2, D1, _, Frame),
      sub_string(Frame, D, SubTextN, _, SubText),
      T is T2 + D
    ; nb_setarg(1, S, TC),
      fail
    ).

seek1_char_left(Text, Char, F1, F) :-
    succ(F2, F1),
    ( sub_string(Text, F2, _, _, Char)
    ->F = F2
    ; seek1_char_left(Text, Char, F2, F)
    ).

seekn_char_right(0, _, _, _, T, T) :- !.
seekn_char_right(N, Text, L, Char, T1, T) :-
    S = s(0),
    ( seek_sub_string(Text, Char, 1, L, T1, T2),
      arg(1, S, N1),
      succ(N1, N2),
      ( N2 = N
      ->!,
        succ(T2, T)
      ; nb_setarg(1, S, N2),
        fail
      )
    ).
