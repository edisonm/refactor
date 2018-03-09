/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
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

:- module(fix_termpos, [fix_termpos/1,
                        fix_subtermpos/1,
                        term_innerpos/4
                       ]).

:- use_module(library(apply)).
:- use_module(library(ref_replace)).

%!  term_innerpos(OFrom, OTo, InnerFrom, InnerTo)
%
%   Contains the inner positions of a term, that exclude comments and some extra
%   parenthesis.
%
:- dynamic term_innerpos/4.

%!  fix_termpos(@TermPos) is det
%
%   Applies fix_subtermpos recursivelly and extends the boundaries of the first
%   term position from the first comment up to just before the ending dot.
%
%   Due to performance concerns, this predicate makes a destructive assignment
%   in the TermPos argument, but preserve the extra positions in the
%   term_innerpos/4 predicate.
%
%   @see fix_subtermpos/1
%
fix_termpos(TermPos) :-
    retractall(term_innerpos(_, _, _, _)),
    fix_subtermpos_rec(TermPos),
    fix_termouterpos(TermPos).

%!  fix_termouterpos(@TermPos) is det
%
%   Extends the boundaries of the first term position from the first comment up
%   to just before the ending dot.
%
fix_termouterpos(TermPos) :-
    arg(1, TermPos, From),
    ( refactor_context(comments, Comments)
    ->true
    ; Comments = []
    ),
    ( Comments = [Pos-_|_],
      stream_position_data(char_count, Pos, From1),
      From1 < From
    ->OuterFrom = From1
    ; OuterFrom = From
    ),
    arg(2, TermPos, To),
    ( append(_, [Pos-Comment], Comments),
      stream_position_data(char_count, Pos, To1),
      string_length(Comment, CL),
      To2 is To1 + CL,
      To2 > To
    ->To3 = To2
    ; To3 = To
    ),
    refactor_context(text, Text),
    string_length(Text, L),
    once(seek_sub_string(Text, ".", 1, L, To3, OuterTo)),
    nb_setarg(1, TermPos, OuterFrom),
    nb_setarg(2, TermPos, OuterTo),
    assertz(term_innerpos(OuterFrom, OuterTo, From, To)).

%!  fix_subtermpos(@TermPos) is det
%
%   Takes a subterm position, as returned by the subterm_positions option of
%   read_term/2 and increases its precision, avoiding some minor mistmatches
%   with the text, that for a refactoring tool is instrumental.  This method
%   also ensures that the minimal required parenthesis enclosing a term are
%   contained in its scope, widening the positions 1 and 2 of the given term
%   position specifier. The current implementation is aware of comments and
%   extra parenthesis, asserting such information in term_innerpos/4 facts.
%
%   @tbd This implementation have performance issues, needs optimization.
%
fix_subtermpos(Pos) :- var(Pos), !.
fix_subtermpos(Pos) :-
    retractall(term_innerpos(_, _, _, _)),
    fix_subtermpos_rec(Pos),
    arg(1, Pos, From),
    arg(2, Pos, To),
    assertz(term_innerpos(From, To, From, To)).

fix_subtermpos_rec(Pos) :- var(Pos), !. % Nothing to fix
fix_subtermpos_rec(Pos) :-
    Pos = term_position(From0, To0, FFrom, FTo, PosL),
    !,
    fix_subtermpos_from_to(From0, To0, FFrom, FTo, From, To, PosL),
    nb_setarg(1, Pos, From),
    nb_setarg(2, Pos, To).
fix_subtermpos_rec(Pos) :-
    Pos = key_value_position(From0, To0, SFrom, STo, _, KPos, VPos),
    !,
    fix_subtermpos_from_to(From0, To0, SFrom, STo, From, To, [KPos, VPos]),
    nb_setarg(1, Pos, From),
    nb_setarg(2, Pos, To).
fix_subtermpos_rec(_-_).
fix_subtermpos_rec(string_position(_, _)).
fix_subtermpos_rec(brace_term_position(From, _, Arg)) :-
    refactor_context(text, Text),
    succ(From, From1),
    fix_termpos_from_left(Text, Arg, From1, _).
fix_subtermpos_rec(parentheses_term_position(From, _, Arg)) :-
    refactor_context(text, Text),
    succ(From, From1),
    fix_termpos_from_left(Text, Arg, From1, _).
% Note: don't assume that a list is between brackets [], because this is also
% used to process list of clauses:
fix_subtermpos_rec(list_position(From, To, Elms, Tail)) :-
    refactor_context(text, Text),
    foldl(fix_termpos_from_left_comm(Text), Elms, From, To1),
    ( Tail = none
    ->true
    ; once(seek_sub_string(Text, "|", 1, To, To1, ToL)),
      succ(ToL, FromT),
      fix_termpos_from_left(Text, Tail, FromT, _)
    ).
fix_subtermpos_rec(map_position(_, _, _, TypeTo, KVPos)) :-
    refactor_context(text, Text),
    succ(TypeTo, TypeTo1),
    foldl(fix_termpos_from_left_comm(Text), KVPos, TypeTo1, _).

comment_bound(CommentL, From, To) :-
    member(Pos-Text, CommentL),
    stream_position_data(char_count, Pos, From),
    string_length(Text, Length),
    nl_mark_end(Text, Delta),
    To is Length + Delta + From.

nl_mark_end(Text, Delta) :-
    ( string_code(1, Text, 0'%)
    ->Delta = 1
    ; Delta = 0
    ).

comment_bound(From, To) :-
    refactor_context(comments, CommentL),
    comment_bound(CommentL, From, To).

rcomment_bound(From, To) :-
    refactor_context(comments, CommentL),
    reverse(CommentL, CommentR),
    comment_bound(CommentR, From, To).

count_sub_string(Text, From0, To0, SubText, SubTextN, From, To, N) :-
    ( seek_sub_string(Text, SubText, SubTextN, To0, From0, From1)
    ->From = From1,
      To1 is From1 + SubTextN,
      ( To1 =< To0
      ->S = s(1, To1),
        forall(seek_sub_string(Text, SubText, SubTextN, To0, To1, To2),
               ( arg(1, S, N1),
                 succ(N1, N2),
                 nb_setarg(1, S, N2),
                 To3 is To2 + SubTextN,
                 nb_setarg(2, S, To3)
               )),
        arg(1, S, N),
        arg(2, S, To)
      ; N = 1,
        To = To1
      )
    ; From = To0,
      To = From0,
      N = 0
    ).

comment_bound(From0, To0, From, To) :-
    comment_bound(FromC, ToC),
    ( FromC < To0
    ->true
    ; FromC = To0
    ->!
    ; !,
      fail
    ),
    ( ToC < To0
    ->To = ToC
    ; !,
      To = To0
    ),
    ( From0 =< FromC
    ->From = FromC
    ; From0 =< ToC
    ->From = From0
    ).

seek_sub_string(Text, SubText, SubTextN, F, T0, T) :-
    S = s(T0),
    ( comment_bound(T0, F, FC, TC)
    ; FC = F,
      TC = F
    ),
    arg(1, S, T1),
    ( D1 is FC - T1,
      D1 > 0,
      sub_string(Text, T1, D1, _, Frame),
      sub_string(Frame, D, SubTextN, _, SubText),
      T is T1 + D
    ; nb_setarg(1, S, TC),
      fail
    ).

seek1_parenthesis_left(Text, F0, F) :-
    comment_bound(F1, F0 ),
    !,
    seek1_parenthesis_left(Text, F1, F).
seek1_parenthesis_left(Text, F0, F) :-
    succ(F1, F0),
    ( sub_string(Text, F1, _, _, "(")
    ->F = F1
    ; seek1_parenthesis_left(Text, F1, F)
    ).

seekn_parenthesis_left(0,  _,    F,  F) :- !.
seekn_parenthesis_left(N0, Text, F0, F) :-
    N0>0,
    seek1_parenthesis_left(Text, F0, F1),
    succ(N, N0),
    seekn_parenthesis_left(N, Text, F1, F).

include_comments_left(Text, To, From) :-
    S = s(To),
    ( rcomment_bound(FromC, ToC),
      arg(1, S, From0 ),
      ToC =< From0,
      ( L is From0 - ToC,
        sub_string(Text, ToC, L, _, Text1),
        \+ ( sub_string(Text1, _, 1, _, Char),
             \+ member(Char, [" ", "\t", "\n"])
           )
      ->nb_setarg(1, S, FromC),
        fail
      ; ToC = From0
      ->nb_setarg(1, S, FromC),
        !,
        fail
      ; !,
        fail
      )
    ->true
    ; true
    ),
    arg(1, S, From).

include_comments_right(Text, From, To) :-
    S = s(From),
    ( comment_bound(FromC, ToC),
      arg(1, S, To0 ),
      To0 =< FromC,
      ( L is FromC - To0,
        sub_string(Text, To0, L, _, Text1),
        \+ ( sub_string(Text1, _, 1, _, Char),
             \+ member(Char, [" ", "\t", "\n"])
           )
      ->nb_setarg(1, S, ToC),
        fail
      ; To0 = FromC
      ->nb_setarg(1, S, ToC),
        !,
        fail
      ; !,
        fail
      )
    ->true
    ; true
    ),
    arg(1, S, To).

seekn_parenthesis_right(0, _, _, T, T) :- !.
seekn_parenthesis_right(N, Text, L, T0, T) :-
    S = s(0),
    ( seek_sub_string(Text, ")", 1, L, T0, T1),
      arg(1, S, N0),
      succ(N0, N1),
      ( N1 = N -> !,
        succ(T1, T)
      ; nb_setarg(1, S, N1),
        fail
      )
    ).

fix_boundaries_from_right(Text, Pos, To0, From2, To2, From, To) :-
    arg(2, Pos, To1),
    ( To0 < To1
    ->RL is To1 - To0,
      sub_string(Text, To0, RL, _, TextL),
      with_termpos(refactor_message(warning, format("Misplaced text --> `~w'", [TextL])), Pos)
    ; true
    ),
    count_sub_string(Text, To1, To0, ")", 1, _, To2, N),
    include_comments_right(Text, To2, To),
    arg(1, Pos, From1),
    seekn_parenthesis_left(N, Text, From1, From2),
    From = From2.

fix_termpos_from_right(Text, To0, Pos ) :-
    fix_subtermpos_rec(Pos),
    fix_boundaries_from_right(Text, Pos, To0, From2, To2, From, To),
    nb_setarg(1, Pos, From),
    nb_setarg(2, Pos, To),
    assertz(term_innerpos(From, To, From2, To2)).

fix_termpos_from_left(Text, Pos, From0, To) :-
    fix_subtermpos_rec(Pos),
    fix_boundaries_from_left(Text, Pos, From0, From2, From, To),
    nb_setarg(1, Pos, From),    % if using From2 and To2, comments not included
    nb_setarg(2, Pos, To),      % TODO: make this parameterizable
    assertz(term_innerpos(From, To, From2, To)).

fix_termpos_from_left_comm(Text, Pos, From0, To) :-
    fix_subtermpos_rec(Pos),
    fix_boundaries_from_left(Text, Pos, From0, From2, From, To2),
    include_comments_right(Text, To2, To),
    nb_setarg(1, Pos, From),  % if using From2 and To2, comments not included
    nb_setarg(2, Pos, To),    % TODO: make this parameterizable
    assertz(term_innerpos(From, To, From2, To2)).

fix_boundaries_from_left(Text, Pos, From0, From2, From, To) :-
    arg(1, Pos, From1),
    ( From1 < From0 ->
      RL is From0 - From1,
      sub_string(Text, From1, RL, _, TextL),
      with_termpos(
          refactor_message(
              warning,
              format("Misplaced text <-- `~w' (~w)",
                     [TextL,
                      fix_boundaries_from_left(_, Pos, From0, From2, From, To)])),
          Pos)
    ; true
    ),
    count_sub_string(Text, From0, From1, "(", 1, From2, _, N),
    include_comments_left(Text, From2, From),
    arg(2, Pos, To1),
    string_length(Text, L),
    seekn_parenthesis_right(N, Text, L, To1, To).

fix_subtermpos_from_to(From0, To0, FFrom, FTo, From, To, PosL) :-
    refactor_context(text, Text),
    sub_string(Text, FTo, 1, _, Char),
    ( PosL = [LPos, RPos ],
      arg(2, LPos, ToL),
      ToL =< FFrom
    ->fix_termpos_from_right(Text, FFrom, LPos),
      fix_termpos_from_left(Text, RPos, FTo, _),
      arg(1, LPos, From),
      arg(2, RPos, To)
    ; PosL = [Pos],
      arg(1, Pos, FromR),
      FTo =< FromR,
      Char \= "("
    ->fix_termpos_from_left(Text, Pos, FTo, _),
      From = From0,
      arg(2, Pos, To)
    ; succ(FTo, FTo1),
      foldl(fix_termpos_from_left_comm(Text), PosL, FTo1, _),
      From = From0,
      To = To0
    ).
