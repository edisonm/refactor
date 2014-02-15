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

:- module(substitute, [substitute/5, is_subterm/2]).

:- meta_predicate substitute(2, ?, ?, ?, ?).
substitute(Comp, Subs, Find, Term0, Term) :-
    ( call(Comp, Term0, Find)
    ->Term = Subs
    ; compound(Term0)
    ->functor(Term0, F, A),
      functor(Term,  F, A),
      substitute(1, Comp, Subs, Find, Term0, Term)
    ; Term = Term0
    ).

substitute(N, Comp, Subs, Find, Term0, Term) :-
    arg(N, Term0, Arg0),
    !,
    substitute(Comp, Subs, Find, Arg0, Arg),
    arg(N, Term, Arg),
    succ(N, N1),
    substitute(N1, Comp, Subs, Find, Term0, Term).
substitute(_, _, _, _, _, _).

is_subterm(SubTerm, Term) :-
    substitute(==, Var, SubTerm, Term, Term1),
    occurrences_of_var(Var, Term1, N),
    !,
    N > 0.
