/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(fix_termpos, [fix_termpos/2,
			fix_subtermpos/2,
			term_outerpos/4
		       ]).

:- use_module(library(maplist_dcg)).

%% term_outerpos(From, To, OuterFrom, OuterTo)
%
%  Contains the position of the surrounding of a term, that includes comments
%  and extra parenthesis up to the begining of other terms or space characters.
%
:- dynamic term_outerpos/4.

%% fix_termpos(+TermPos, -FixedTermPos) is det
%
%  Applies fix_subtermpos recursivelly and extends the boundaries of the first
%  term position from the first comment up to just before the ending dot.
%
%  @see fix_subtermpos/2
%
fix_termpos(TermPos, FTermPos) :-
    retractall(term_outerpos(_, _, _, _)),
    fix_subtermpos(TermPos, FTermPos),
    get_term_outerpos(FTermPos).

%% fix_termpos(+TermPos, -FixedTermPos) is det
%
%  Extends the boundaries of the first term position from the first comment up
%  to just before the ending dot.
%
get_term_outerpos(TermPos) :-
    arg(1, TermPos, From),
    ( b_getval(refactor_comments, [Pos-_|_]),
      stream_position_data(char_count, Pos, From1),
      From1 < From
    ->OuterFrom = From1
    ; OuterFrom = From
    ),
    arg(2, TermPos, To),
    b_getval(refactor_text, Text),
    string_length(Text, L),
    once(seek_sub_string(Text, ".", 1, L, To, OuterTo)),
    assertz(term_outerpos(From, To, OuterFrom, OuterTo)).

%% fix_subtermpos(+TermPos, -FixedTermPos) is det
%
%  Takes a subterm position, as returned by the subterm_positons option of
%  read_term/2 and increases its precision, avoiding some minor mistmatches with
%  the text, that for a refactoring tool is instrumental.  This method also
%  ensures that the parenthesis enclosing a term are contained in its scope,
%  widening the positions 1 and 2 of the given term position specifier. The
%  current implementation is aware of comments and extra parenthesis, asserting
%  such information in term_outerpos/4 facts.
%
%  @tbd This implementation have performance issues, needs optimization.
%
fix_subtermpos(term_position(From0, To0, FFrom, FTo, PosL0 ),
	       term_position(From,  To,  FFrom, FTo, PosL)) :-
    fix_subtermpos_rec(From0, To0, FFrom, FTo, From, To, PosL0, PosL).
fix_subtermpos(From-To, From-To).
fix_subtermpos(string_position(From, To),
	       string_position(From, To)).
fix_subtermpos(brace_term_position(From, To, Arg0 ),
	       brace_term_position(From, To, Arg)) :-
    b_getval(refactor_text, Text),
    succ(From, From1),
    fix_termpos_from_left(Text, Arg0, Arg, From1, _).
fix_subtermpos(list_position(From, To, Elms0, Tail0 ),
	       list_position(From, To, Elms,  Tail)) :-
    b_getval(refactor_text, Text),
    ( Tail0 = none ->
      succ(From, From1),
      maplist_dcg(fix_termpos_from_left(Text), Elms0, Elms, From1, _),
      Tail = none
    ; succ(From, From1),
      maplist_dcg(fix_termpos_from_left(Text), Elms0, Elms, From1, To1),
      succ(ToT, To),
      once(seek_sub_string(Text, "|", 1, ToT, To1, ToL)),
      succ(ToL, FromT),
      fix_termpos_from_left(Text, Tail0, Tail, FromT, _)
    ).
fix_subtermpos(map_position(From, To, TypeFrom, TypeTo, KVPos0 ),
	       map_position(From, To, TypeFrom, TypeTo, KVPos)) :-
    b_getval(refactor_text, Text),
    succ(TypeTo, TypeTo1),
    maplist_dcg(fix_termpos_from_left(Text), KVPos0, KVPos, TypeTo1, _).
fix_subtermpos(key_value_position(From0, To0, SFrom, STo, Key, KPos0, VPos0),
	       key_value_position(From,  To,  SFrom, STo, Key, KPos , VPos)) :-
    fix_subtermpos_rec(From0, To0, SFrom, STo, From, To,
		       [KPos0, VPos0 ], [KPos, VPos]).

match_comment(CharPos, Length) :-
    b_getval(refactor_comments, CommentL),
    member(Pos-Text, CommentL),
    stream_position_data(char_count, Pos, CharPos),
    string_length(Text, Length).

comment_bound(From, To) :-
    b_getval(refactor_comments, CommentL),
    member(Pos-Text, CommentL),
    stream_position_data(char_count, Pos, From),
    string_length(Text, Length),
    To is Length + From.

count_parenthesis_right(Text, F, T0, T, N0, N) :-
    seek_sub_string(Text, ")", 1, F, T0, T1),
    !,
    succ(N0, N1),
    T2 is T1 + 1,		% length(")")expand
    count_parenthesis_right(Text, F, T2, T, N1, N).
count_parenthesis_right(_, _, T, T, N, N).

comment_bound(From, To, FromC, ToC) :-
    comment_bound(FromC, ToC),
    From < FromC,
    ( FromC < To
    ->true
    ; !,
      fail
    ).

seek_sub_string(Text, SubText, SubTextN, F, T0, T) :-
    S = s(T0),
    ( comment_bound(T0, F, FC, TC)
    ; FC = F,
      TC = F
    ),
    arg(1, S, T1),
    ( D1 is FC - T1,
      sub_string(Text, T1, D1, _, Frame),
      sub_string(Frame, D, SubTextN, _, SubText),
      T is T1 + D
    ; nb_setarg(1, S, TC),
      fail
    ).

seek1_parenthesis_left(Text, F0, F) :-
    match_comment(F1, D),
    F0 =:= F1 + D,
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

include_comments_right(Text, From, To) :-
    S = s(From),
    ( comment_bound(FromC, ToC),
      arg(1, S, To0 ),
      ( To0 < FromC,
	L is FromC - To0,
	sub_string(Text, To0, L, _, Text1),
	\+ ( sub_string(Text1, _, 1, _, Char),
	     \+ member(Char, [" ", "\t", "\n"])
	   )
      ->nb_setarg(1, S, ToC),
	fail
      ; !,
	fail
      )
    ->true
    ; true
    ),
    arg(1, S, To).

count_parenthesis_left(Text, D0, F0, F, T, N0, N) :-
    F1 is F0 - D0,
    T =< F1,
    match_comment(F2, D),
    F1 =:= F2 + D,
    !,
    D1 is D0 + D,
    count_parenthesis_left(Text, D1, F0, F, T, N0, N).
count_parenthesis_left(Text, D0, F0, F, T, N0, N) :-
    F1 is F0 - D0,
    T =< F1,
    ( sub_string(Text, F1, 1, _, "(")
    ->succ(N0, N1),
      F2 = F1,
      D = 1
    ; N0 = N1,
      F2 = F0,
      succ(D0, D)
    ),
    !,
    count_parenthesis_left(Text, D, F2, F, T, N1, N).
count_parenthesis_left(_, _, F, F, _, N, N).

seek1_parenthesis_right(Text, L, T0, T) :-
    match_comment(T0, D),
    !,
    T1 is T0 + D,
    seek1_parenthesis_right(Text, L, T1, T).
seek1_parenthesis_right(Text, L, T0, T) :-
    T0 < L,
    succ(T0, T1),
    ( sub_string(Text, T0, _, _, ")")
    ->T = T1
    ; seek1_parenthesis_right(Text, L, T1, T)
    ).

seekn_parenthesis_right(0,  _,    _, T,  T) :- !.
seekn_parenthesis_right(N0, Text, L, T0, T) :-
    N0>0,
    seek1_parenthesis_right(Text, L, T0, T1),
    succ(N, N0 ),
    seekn_parenthesis_right(N, Text, L, T1, T).

fix_boundaries_from_right(Text, Pos, From0, To0, From2, To2, From, To) :-
    arg(2, Pos, To1),
    ( To0 < To1
    ->RL is To1 - To0,
      sub_string(Text, To0, RL, _, TextL),
      print_message(warning, format("Misplaced text --> `~w'", [TextL]))
    ; true
    ),
    count_parenthesis_right(Text, To0, To1, To2, 0, N),
    include_comments_right(Text, To2, To),
    arg(1, Pos, From1),
    seekn_parenthesis_left(N, Text, From1, From2),
    include_comments_left(Text, From0, From2, From).

fix_termpos_from_right(Text, FFrom, Pos0, Pos, From0 ) :-
    fix_subtermpos(Pos0, Pos),
    fix_boundaries_from_right(Text, Pos, From0, FFrom, From2, To2, From, To),
    nb_setarg(1, Pos, From2),
    nb_setarg(2, Pos, To2),
    assertz(term_outerpos(From2, To2, From, To)).

fix_termpos_from_left(Text, Pos0, Pos, FTo, To) :-
    fix_subtermpos(Pos0, Pos),
    fix_boundaries_from_left(Text, Pos, FTo, From2, To2, From, To),
    nb_setarg(1, Pos, From2),
    nb_setarg(2, Pos, To2),
    assertz(term_outerpos(From2, To2, From, To)).

fix_boundaries_from_left(Text, Pos, From0, From2, To2, From, To) :-
    arg(1, Pos, From1),
    ( From1 < From0 ->
      RL is From0 - From1,
      sub_string(Text, From1, RL, _, TextL),
      print_message(warning, format("Misplaced text <-- `~w'", [TextL]))
    ; true
    ),
    count_parenthesis_left(Text, 1, From1, From2, From0, 0, N),
    include_comments_left(Text, From0, From2, From),
    arg(2, Pos, To1),
    string_length(Text, L),
    seekn_parenthesis_right(N, Text, L, To1, To2),
    include_comments_right(Text, To2, To).

fix_subtermpos_rec(From0, To0, FFrom, FTo, From, To, PosL0, PosL) :-
    b_getval(refactor_text, Text),
    sub_string(Text, FTo, 1, _, Char),
    ( PosL0 = [LPos0, RPos0 ],
      arg(2, LPos0, ToL),
      ToL =< FFrom
    ->fix_termpos_from_right(Text, FFrom, LPos0, LPos, From0),
      fix_termpos_from_left(Text, RPos0, RPos, FTo, _),
      PosL  = [LPos, RPos],
      arg(1, LPos, From),
      arg(2, RPos, To)
    ; PosL0 = [Pos0 ],
      arg(1, Pos0, FromR),
      FTo =< FromR,
      Char \= "("
    ->fix_termpos_from_left(Text, Pos0, Pos, FTo, _),
      PosL = [Pos],
      From = From0,
      arg(2, Pos, To)
    ; succ(FTo, FTo1),
      maplist_dcg(fix_termpos_from_left(Text), PosL0, PosL, FTo1, _),
      From = From0,
      To = To0
    ).

include_comments_left(Text, From, To, NFrom) :-
    S = s(From),
    T = s(From),
    ( ( comment_bound(FromC, ToC),
	From =< FromC,
	ToC =< To
      ; FromC = To,
	ToC = To
      ),
      arg(1, T, To0 ),
      nb_setarg(1, T, ToC),
      ( L is FromC - To0,
	L > 0,
	sub_string(Text, To0, L, _, Text1),
	\+ ( sub_string(Text1, _, 1, _, Char),
	     \+ member(Char, [" ", "\t", "\n"])
	   )
      ->fail
      ; nb_setarg(1, S, FromC),
	fail
      )
    ->true
    ; true
    ),
    arg(1, S, NFrom).
