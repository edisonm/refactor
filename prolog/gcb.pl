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

:- module(gcb, [greatest_common_binding/7,
		greatest_common_binding/9]).

:- use_module(library(substitute), [substitute_value/4]).

greatest_common_binding(Term0, Into0, Term, Into, Skip) -->
    greatest_common_binding(Term0, _, Term0, Into0, Term, Into, Skip).

greatest_common_binding(SubTerm0, SubTerm, Term0, Into0, Term, Into, Skip) -->
    ( {nonvar(SubTerm0 ), nonvar(Into0 ), \+memberchk(SubTerm0, Skip)}
    ->( { substitute_value(SubTerm0, Var, Into0, Into1),
	  Into0\==Into1}
      ->{substitute_value(SubTerm0, Var, Term0, Term1)},
	[Var=SubTerm]
      ; {Term1=Term0, Into1 = Into0 }
      ),
      ( {compound(SubTerm0 )},
        greatest_common_binding(1, SubTerm0, SubTerm, Term1, Into1, Term, Into, Skip),
	{Into1\==Into}
      ->[]
      ; {SubTerm=SubTerm0, Term=Term1, Into=Into1 }
      )
    ; {SubTerm=SubTerm0, Term=Term0, Into=Into0 }
    ).

greatest_common_binding(N, SubTerm0, SubTerm, Term0, Into0, Term, Into, Skip) -->
    {arg(N, SubTerm0, Arg)},
    !,
    pick_tail(Tail),
    greatest_common_binding(Arg, _, Term0, Into0, Term1, Into1, Skip),
    {substitute_olist(Tail, Term1, Term2),
     substitute_olist(Tail, SubTerm0, SubTerm1),
     succ(N, N1)},
    greatest_common_binding(N1, SubTerm1, SubTerm, Term2, Into1, Term, Into, Skip).
greatest_common_binding(_, SubTerm, SubTerm, Term, Into, Term, Into, _) --> [].

% Fixpoint algorithm:
substitute_olist(SubstList, Term0, Term) :-
    ( substitute_olist_(SubstList, Term0, Term1),
      Term0 \== Term1 ->
      substitute_olist(SubstList, Term1, Term)
    ; Term0 = Term
    ).

substitute_olist_(Tail) --> {var(Tail)}, !.
substitute_olist_([Var=Val|Tail]) -->
    substitute_value(Val, Var),
    substitute_olist_(Tail).

pick_tail(Tail, Tail, Tail).
