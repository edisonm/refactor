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
	  [ get_term_info/7,
	    get_term_info_file/5
	  ]).

:- use_module(library(prolog_source)).
:- use_module(library(module_files)).

:- meta_predicate get_term_info(+, ?, ?, 1, -, -, +).
get_term_info(M, Pattern, Term, AllChk, File, In, Options) :-
    module_files(M, Files),
    member(File, Files),
    call(AllChk, File),
    get_term_info_file(Pattern, Term, File, In, Options).

fix_exception(error(Error, stream(_,  Line, Row, Pos)), File,
	      error(Error, file(File, Line, Row, Pos))) :- !.
fix_exception(E, _, E).

ti_open_source(Path, In) :-
    b_setval(ti_open_source, yes),
    prolog_open_source(Path, In),
    b_setval(ti_open_source, no).

get_term_info_file(Pattern, Term, File, In, Options) :-
    prolog_canonical_source(File, Path),
    print_message(informational, format("Reading ~w", Path)),
    catch(setup_call_cleanup(ti_open_source(Path, In),
			     ( get_term_info_fd(In, Pattern, Term, Options)
			     ; fail % don't close In up to the next iteration
			     ),
			     prolog_close_source(In)),
	  E0, ( fix_exception(E0, Path, E),
		print_message(error, E),
		fail
	      )).

get_term_info_fd(In, Pattern, Term, Options) :-
    repeat,
    read_term(In, Term, Options),
    ( Term == end_of_file ->
      !,
      fail
    ; subsumes_term(Pattern, Term)
    ).

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
