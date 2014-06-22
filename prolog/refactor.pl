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

:- module(refactor, [op(1,xfy,($@))]).

:- reexport(library(ref_command)).
:- use_module(library(ref_pending)).
:- use_module(library(ref_expanders), []).
:- use_module(library(ref_scenarios), []).

term_expansion((:- comm_commands(M)), ClauseL) :-
    findall(Clause,
	    ( current_predicate(M:F/A),
	      functor(H, F, A),
	      predicate_property(M:H, exported),
	      ( Clause = (:- export(F/A))
	      ; predicate_property(M:H, meta_predicate Meta),
		Clause = (:- meta_predicate Meta)
	      ; Clause = (H :- apply_command(M:H), rdiff_q)
	      )
	    ), ClauseL).

:- comm_commands(ref_expanders).
:- comm_commands(ref_scenarios).
