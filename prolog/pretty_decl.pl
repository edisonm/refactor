/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.

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

:- module(pretty_decl,
	  [pretty_decl/2,
	   pretty_decl/3,
	   pretty_decl/4]).

pretty_decl(Decl, PDecl, Id, Next) :-
    pretty_decl(Decl, PDecl, Id),
    succ(Id, Next).

pretty_decl(Decl, PDecl) :- pretty_decl(Decl, PDecl, 1).

pretty_decl((:- use_module(A, L)),
	    (:- $@(use_module('$POS'('$1'(Id), A),
			      '$NLID'('$LISTB,NL'(L, '$1'(Id)+1), '$1'(Id))))), Id) :- !.
pretty_decl((:- module(M, L)),
	    (:- $@(module('$POS'('$1'(Id), M),
			  '$NLID'('$LISTB,NL'(L, '$1'(Id)+1), '$1'(Id))))), Id) :- !.
pretty_decl((:- reexport(A, L)),
	    (:- $@(reexport('$POS'('$1'(Id), A),
			    '$NLID'('$LISTB,NL'(L, '$1'(Id)+1), '$1'(Id))))), Id) :- !.
pretty_decl((:- export(L)), (:- $@(export('$NLID'('$LIST,NL'(L),'$OUTPOS')))), _) :- !.
pretty_decl((:- M:export(L)), (:- $@(M:export('$NLID'('$LIST,NL'(L,'$OUTPOS'),'$OUTPOS'+1)))), _) :- !.
pretty_decl(Decl, Decl, _).
