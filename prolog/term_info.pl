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
	  [ get_term_info/4,
	    get_term_info/5
	  ]).
:- use_module(library(prolog_source)).
:- use_module(library(included_files)).

% NOTE: Files are not uniques, therefore I have to sort
module_files(M, Files) :-
    module_file_list(M, UFilesL),
    append(UFilesL, UFiles),
    sort(UFiles, Files).

module_file_list(M, Files) :-
    findall(F, module_file_1(M, F), UFiles),
    sort(UFiles, Files0),
    included_files(Files0, Files, [Files0]).

module_file_1(M, File) :-
    module_property(M, file(File)).
module_file_1(M, File) :-
    '$load_context_module'(File, M, _),
    \+ module_property(_, file(File)).

get_term_info(M, Pattern, File, Options) :-
	get_term_info(M, Pattern, Term, File, Options),
	Term = Pattern.

get_term_info(M, Pattern, Term, File, Options) :-
    module_files(M, Files),
    member(File, Files),
    get_term_info_file(Pattern, Term, File, [module(M)|Options]).

get_term_info_file(Pattern, Term, File, Options) :-
	catch(setup_call_cleanup(
		  ( prolog_canonical_source(File, Path),
		    prolog_open_source(Path, In)
		  ),
		  get_term_info_fd(In, Pattern, Term, Options),
		  prolog_close_source(In)),
	  E, (print_message(error, E), fail)).

get_term_info_fd(In, Pattern, Term, Options) :-
    repeat,
    prolog_read_source_term(In, Term, _Expanded, Options),
    ( Term == end_of_file ->
      !,
      fail
    ; subsumes_term(Pattern, Term)
    ).
