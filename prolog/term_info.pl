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

:- module(term_info,
	  [ get_term_info/6,
	    with_source_file/2,
	    fetch_term_info/4
	  ]).

:- use_module(library(apply)).
:- use_module(library(prolog_source)).
:- use_module(library(clambda)).
:- use_module(library(module_files)).

:- meta_predicate
    with_source_file(+, 1),
    get_term_info(+, ?, ?, 1, -, +),
    transverse_apply_2(1, +, ?, ?, ?).

get_term_info(M, Pattern, Term, AllChk, File, Options) :-
    module_files(M, Files),
    member(File, Files),
    call(AllChk, File),
    get_term_info_file(Pattern, Term, File, Options).

fix_exception(error(Error, stream(_,  Line, Row, Pos)), File,
	      error(Error, file(File, Line, Row, Pos))) :- !.
fix_exception(E, _, E).

ti_open_source(Path, In) :-
    b_setval(ti_open_source, yes),
    prolog_open_source(Path, In),
    b_setval(ti_open_source, no).

with_source_file(File, Goal) :-
    prolog_canonical_source(File, Path),
    print_message(informational, format("Reading ~w", Path)),
    catch(setup_call_cleanup(ti_open_source(Path, In),
			     call(Goal, In),
			     prolog_close_source(In)),
	  E0, ( fix_exception(E0, Path, E),
		print_message(error, E),
		fail
	      )).

fetch_term_info(Pattern, Term, Options, In) :-
    ( ( option(line(Line), Options),
	nonvar(Line)
      ->seek(In, 0, bof, _),
	prolog_source:seek_to_line(In, Line),
	once(get_term_info_fd(In, Pattern, Term, Options))
      ; get_term_info_fd(In, Pattern, Term, Options)
      )
    ; fail			% dont close In up to the next iteration
    ).

get_term_info_file(Pattern, Term, File, Options) :-
    with_source_file(File, fetch_term_info(Pattern, Term, Options)).

get_term_info_fd(In, PatternL, TermL, OptionL0 ) :-
    is_list(PatternL), !,
    foldl(\ (H-D)^O0^O^select_option(H, O0, O, D),
		[subterm_positions(TermPos)-TermPos,
		 comments(Comments)-Comments,
		 variable_names(VN)-VN
		], OptionL0, OptionT),
    PatternL = [_|PatternT],
    length(PatternT, TN),
    length(TA, TN),
    maplist([In, OptionT] +\
	   T^P^V^C^get_term_info_each(In, OptionT, [T, P, V, C]), TA, PA, VA, CA),
    maplist(append, [TA, PA, VA, CA], TPVCT, TPVCH),
    transverse_apply_2(get_term_info_each(In, OptionT), TPVCH, TPVCT,
		       [TermL, TermPosL, VNL, CommentsL], [_, TermPosE, _, _]),
    maplist(subsumes_term, PatternL, TermL),
    append(CommentsL, Comments),
    append(VNL, VN),
    TermPosL = [TermPosI|_],
    arg(2, TermPosE, To),
    findall(From, ( Comments = [StreamPos-_|_],
		    stream_position_data(char_count, StreamPos, From)
		  ; arg(1, TermPosI,  From)
		  ),
	    FromL),
    min_list(FromL, From),
    TermPos = list_position(From, To, TermPosL, none).
get_term_info_fd(In, Pattern, Term, Options0 ) :-
    should_set_line(SetLine, Options0, Options),
    repeat,
      '$set_source_module'(M, M),
      read_term(In, Term, [module(M)|Options]),
      set_line(SetLine),
      ( Term == end_of_file ->
	!,
	fail
      ; ( member(ModDecl, [(:- module(CM, _)), (:- module(CM, _, _))]),
	  subsumes_term(ModDecl, Term),
	  Term = ModDecl
	->'$set_source_module'(_, CM)
	; true
	),
	subsumes_term(Pattern, Term)
      ).

should_set_line(posline(Pos, Line), Options, [term_position(Pos)|Options]) :-
    option(line(Line), Options), !.
should_set_line(no, Options, Options).

set_line(no).
set_line(posline(Pos, Line)) :-
    stream_position_data(line_count, Pos, Line).

transverse_apply(_,     ListL,  ListT, ListL, EL, EL) :- maplist(=([]), ListT).
transverse_apply(Apply, ListH0, ListT, ListL, _,  EL) :-
    maplist(\ [_|L]^L^true, ListH0, ListH),
    transverse_apply_2(Apply, ListH, ListT, ListL, EL).

transverse_apply_2(Apply, ListH, ListT0, ListL, EL) :-
    call(Apply, EL0 ),
    maplist(\ E^[E|L]^L^true, EL0, ListT0, ListT),
    transverse_apply(Apply, ListH, ListT, ListL, EL0, EL).

get_term_info_each(In, Options, [T, P, V, C]) :-
    '$set_source_module'(M, M),
    read_term(In, T, [subterm_positions(P), variable_names(V), comments(C), module(M)|Options]),
    T \== end_of_file.

:- public read_terms/3.

read_terms(In, TermOptsL, Options) :-
    read_terms_opts(TermOptsL, In, Options, TermOptsL0, TermOptsT),
    read_terms_opts_rec(In, Options, TermOptsL0, TermOptsT, TermOptsL).

read_terms_opts([],    _,  _,         TermOptsT,           TermOptsT).
read_terms_opts([_|T], In, Options0, [TermOpts|TermOptsL], TermOptsT) :-
    read_term_opts(In, Options0, TermOpts),
    read_terms_opts(T, In, Options0, TermOptsL, TermOptsT).

read_term_opts(In, Options0, Term-Options) :-
    copy_term(Options0, Options),
    read_term(In, Term, Options).

read_terms_opts_rec(_,  _,        TermOptsL,       [],                   TermOptsL).
read_terms_opts_rec(In, Options0, [_|TermOptsL0 ], [TermOpts|TermOptsT], TermOptsL) :-
    read_term_opts(In, Options0, TermOpts),
    read_terms_opts_rec(In, Options0, TermOptsL0, TermOptsT, TermOptsL).
