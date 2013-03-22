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

:- module(term_info, [get_term_info/4]).

% BUG: Files are not uniques
module_files(M, Files) :-
    findall(File, module_file(M, File), UFiles),
    sort(UFiles, Files).

module_file(M, File) :-
    module_file_1(M, File0),
    module_file_2(M, File0, File).

module_file_1(M, File) :-
    module_property(M, file(File)).
module_file_1(M, File) :-
    '$load_context_module'(File, M, _),
    \+ module_property(_, file(File)).

module_file_2(_, File, File).
module_file_2(M, File0, File) :-
    source_file_property(File0, includes(File1,_)),
    module_file_2(M, File1, File).

get_term_info(M, Term, File, Options) :-
    module_files(M, Files),
    member(File, Files),
    get_term_info_file(Term, File, [module(M)|Options]).

get_term_info_file(Term, File, Options) :-
    catch(setup_call_cleanup(open(File, read, In),
			     get_term_info_fd(In, Term, Options),
			     close(In)),
	  E, (print_message(error, E), fail)).

get_term_info_fd(In, Ref, Options) :-
    repeat,
    catch(read_term(In, Term, Options),
	  E, (print_message(error, E), fail)),
    ( Term = end_of_file ->
      !,
      fail
    ; subsumes_term(Ref, Term),
      Ref = Term
    ).

% match_reference(F/A,   Term) :- functor(Term, F, A).
