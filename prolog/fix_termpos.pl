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

:- module(fix_termpos, [fix_termpos/2, fix_subtermpos/2]).

:- use_module(library(maplist_dcg)).

%% fix_termpos(+TermPos, -FixedTermPos) is det
%
%  Applies fix_subtermpos recursivelly and extends the boundaries of a term
%  position from the first comment up to just before the ending dot.
%
%  @see fix_tubtermpos/2
%
fix_termpos(TermPos, FTermPos) :-
    ( b_getval(refactor_comments, [Pos-_|_]),
      stream_position_data(char_count, Pos, From1),
      arg(1, TermPos, From2),
      From1 < From2
    ->nb_setarg(1, TermPos, From1)
    ; true
    ),
    arg(2, TermPos, To1),
    b_getval(refactor_text, Text),
    string_length(Text, L),
    seek_sub_string(Text, ".", L, To1, To),
    nb_setarg(2, TermPos, To),
    fix_bsubtermpos(TermPos, FTermPos).

%% fix_subtermpos(+TermPos, -FixedTermPos) is det
%
%  Similar to fix_bsubtermpos/2, but in this case the bounds don't need to be
%  fixed.
%
fix_bsubtermpos(term_position(From, To, FFrom, FTo, Pos0 ),
		term_position(From, To, FFrom, FTo, Pos)) :-
    fix_bsubtermpos_rec(From, To, FFrom, FTo, Pos0, Pos).
fix_bsubtermpos(From-To, From-To).
fix_bsubtermpos(string_position(From, To),
	       string_position(From, To)).
fix_bsubtermpos(brace_term_position(From, To, Arg0 ),
		brace_term_position(From, To, Arg)) :-
    succ(From, From1),
    succ(To1, To),
    nb_setarg(1, Arg0, From1),
    nb_setarg(2, Arg0, To1),
    fix_bsubtermpos(Arg0, Arg).
fix_bsubtermpos(list_position(From, To, Elms0, Tail0),
		list_position(From, To, Elms,  Tail)) :-
    b_getval(refactor_text, Text),
    ( Tail0 = none ->
      fix_bsubtermpos_canonical(Text, From, To, Elms0, Elms),
      Tail = none
    ; append(_, [Elm0], Elms0),
      arg(2, Elm0, ToE0),
      succ(ToT, To),
      seek_sub_string(Text, "|", ToT, ToE0, ToE),
      succ(ToE, FromT),
      fix_bsubtermpos_canonical(Text, From, FromT, Elms0, Elms),
      nb_setarg(1, Tail0, FromT),
      nb_setarg(2, Tail0, ToT),
      fix_bsubtermpos(Tail0, Tail)
    ).
fix_bsubtermpos(none, none).
fix_bsubtermpos(map_position(From, To, TypeFrom, TypeTo, KVPos0 ),
		map_position(From, To, TypeFrom, TypeTo, KVPos)) :-
    b_getval(refactor_text, Text),
    fix_bsubtermpos_canonical(Text, TypeTo, To, KVPos0, KVPos).
fix_bsubtermpos(key_value_position(From, To, SFrom, STo, Key, KPos0, VPos0),
		key_value_position(From, To, SFrom, STo, Key, KPos , VPos)) :-
    fix_bsubtermpos_rec(From, To, SFrom, STo, [KPos0, VPos0 ], [KPos, VPos]).

%% fix_subtermpos(+TermPos, -FixedTermPos) is det
%
%  Takes a subterm position, as returned by the subterm_positons option of
%  read_term/2 and increases its precision, avoiding some minor mistmatches with
%  the text, that for a refactoring tool is instrumental.  This method also
%  ensures that the parenthesis enclosing a term are contained in its scope,
%  widening the positions 1 and 2 of the given term position specifier. The
%  current implementation is aware of comments.
%
%  @tbd This implementation have performance issues, needs optimization.
%
fix_subtermpos(term_position(From0, To0, FFrom, FTo, Pos0 ),
	       term_position(From,  To,  FFrom, FTo, Pos)) :-
    fix_subtermpos_rec(From0, To0, FFrom, FTo, From, To, Pos0, Pos).
fix_subtermpos(From-To, From-To).
fix_subtermpos(string_position(From, To),
	       string_position(From, To)).
fix_subtermpos(brace_term_position(From, To, Arg0 ),
	       brace_term_position(From, To, Arg)) :-
    fix_subtermpos(Arg0, Arg).
fix_subtermpos(list_position(From, To, Elms0, Tail0),
	       list_position(From, To, Elms,  Tail)) :-
    maplist(fix_subtermpos, Elms0, Elms),
    fix_subtermpos(Tail0, Tail).
fix_subtermpos(none, none).
fix_subtermpos(map_position(From, To, TypeFrom, TypeTo, KVPos0 ),
	       map_position(From, To, TypeFrom, TypeTo, KVPos)) :-
    maplist(fix_subtermpos, KVPos0, KVPos).
fix_subtermpos(key_value_position(From0, To0, SFrom, STo, Key, KPos0, VPos0),
	       key_value_position(From,  To,  SFrom, STo, Key, KPos , VPos)) :-
    fix_subtermpos_rec(From0, To0, SFrom, STo, From, To,
		       [KPos0, VPos0 ], [KPos, VPos]).

fix_termpos_from_right(Text, Pos0, Pos, FFrom, From) :-
    fix_subtermpos(Pos0, Pos),
    arg(2, Pos, To1),
    ( FFrom < To1
    ->RL is To1 - FFrom,
      sub_string(Text, FFrom, RL, _, TextL),
      print_message(warning, format("Misplaced text `~w'", [TextL]))
    ; true
    ),
    count_parenthesis_right(Text, FFrom, To1, To, 0, N),
    arg(1, Pos, From1),
    seekn_parenthesis_left(N, Text, From1, From),
    nb_setarg(1, Pos, From),
    nb_setarg(2, Pos, To).

match_comment(CharPos, Length) :-
    b_getval(refactor_comments, CommentL),
    member(Pos-Text, CommentL),
    stream_position_data(char_count, Pos, CharPos),
    string_length(Text, Length).

count_parenthesis_right(Text, F, T0, T, N0, N) :-
    seek_sub_string(Text, ")", F, T0, T1),
    !,
    succ(N0, N1),
    T2 is T1 + 1,		% length(")")
    count_parenthesis_right(Text, F, T2, T, N1, N).
count_parenthesis_right(_, _, T, T, N, N).

seek_sub_string(Text, SubText, F, T0, T) :-
    T0 =< F,
    ( match_comment(T0, D)
    ->T1 is T0 + D,
      seek_sub_string(Text, SubText, F, T1, T)
    ; sub_string(Text, T0, _, _, SubText)
    ->T = T0
    ; succ(T0, T1),
      seek_sub_string(Text, SubText, F, T1, T)
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

fix_termpos_from_left(Text, Pos0, Pos, Bound, To) :-
    fix_subtermpos(Pos0, Pos),
    arg(1, Pos, From1),
    ( From1 < Bound ->
      RL is Bound - From1,
      sub_string(Text, From1, RL, _, TextL),
      print_message(warning, format("Misplaced text `~w'", [TextL]))
    ; true
    ),
    count_parenthesis_left(Text, 1, From1, From, Bound, 0, N),
    arg(2, Pos, To1),
    seekn_parenthesis_right(N, Text, To1, To),
    nb_setarg(1, Pos, From),
    nb_setarg(2, Pos, To).

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

seek1_parenthesis_right(Text, T0, T) :-
    match_comment(T0, D),
    !,
    T1 is T0 + D,
    seek1_parenthesis_right(Text, T1, T).
seek1_parenthesis_right(Text, T0, T) :-
    succ(T0, T1),
    ( sub_string(Text, T0, _, _, ")")
    ->T = T1
    ; seek1_parenthesis_right(Text, T1, T)
    ).

seekn_parenthesis_right(0,  _,     T,  T) :- !.
seekn_parenthesis_right(N0, Text, T0, T) :-
    N0>0,
    seek1_parenthesis_right(Text, T0, T1),
    succ(N, N0 ),
    seekn_parenthesis_right(N, Text, T1, T).

fix_subtermpos_rec(From0, To0, FFrom, FTo, From, To, PosL0, PosL) :-
    b_getval(refactor_text, Text),
    sub_string(Text, FTo, 1, _, Char),
    ( PosL0 = [LPos0, RPos0 ],
      arg(2, LPos0, ToL),
      ToL =< FFrom
    ->fix_termpos_from_right(Text, LPos0, LPos, FFrom, From),
      fix_termpos_from_left(Text, RPos0, RPos, FTo, To),
      PosL  = [LPos, RPos]
    ; PosL0 = [Pos0 ],
      arg(1, Pos0, FromR),
      FTo =< FromR,
      Char \= "("
    ->fix_termpos_from_left(Text, Pos0, Pos, FTo, To),
      PosL = [Pos],
      From = From0
    ; succ(FTo, FTo1),
      maplist_dcg(fix_termpos_from_left(Text), PosL0, PosL, FTo1, _),
      From = From0,
      To = To0
    ).

fix_bsubtermpos_rec(From, To, FFrom, FTo, PosL0, PosL) :-
    b_getval(refactor_text, Text),
    sub_string(Text, FTo, 1, _, Char),
    ( PosL0 = [LPos0, RPos0 ],
      arg(2, LPos0, ToL),
      ToL =< FFrom
    ->fix_termpos_from_right(Text, LPos0, LPos, FFrom, _From),
      fix_termpos_from_left(Text, RPos0, RPos, FTo, _To),
      PosL  = [LPos, RPos]
    ; PosL0 = [Pos0 ],
      arg(1, Pos0, FromR),
      FTo =< FromR,
      Char \= "("
    ->fix_termpos_from_left(Text, Pos0, Pos, FTo, _To),
      PosL = [Pos]
    ; fix_bsubtermpos_canonical(Text, FTo, To, PosL0, PosL)
    ).

fix_bsubtermpos_canonical(Text, FTo, To, PosL0, PosL) :-
    succ(FTo, FTo1),
    succ(To1, To),
    append(PosL1, [Pos0 ], PosL0 ),
    maplist_dcg(fix_bsubtermpos_arg(Text, To1), PosL1, PosL2, FTo1, From0),
    fix_bsubtermpos_arg(Text, To1, Pos0, Pos, From0, To),
    append(PosL2, [Pos], PosL).

fix_bsubtermpos_arg(Text, N, Pos0, Pos, From0, From) :-
    arg(1, Pos0, From1),
    From2 is min(From0, From1),
    nb_setarg(1, Pos0, From2),
    arg(2, Pos0, To1),
    ( nonvar(From)
    ->succ(To, From)
    ; seek_sub_string(Text, ",", N, To1, To),
      ( succ(To, To2),
	sub_string(Text, To2, 1, _, " ")
      ->succ(To2, From)
      ; succ(To, From)
      )
    ),
    nb_setarg(2, Pos0, To),
    fix_bsubtermpos(Pos0, Pos).
