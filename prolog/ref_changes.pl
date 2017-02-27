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

:- module(ref_changes,
          [pending_change/1,
           pending_change/3,
           index_change/1,
           save_changes/1,
           save_change/2,
           undo_changes/1,
           reset_changes/0
          ]).

:- dynamic
    pending_change/1,
    pending_change/3.

save_change(Index, File-Content) :-
    asserta(pending_change(Index, File, Content)).

index_change(Index) :-
    (pending_change(Index0) -> succ(Index0, Index) ; Index = 1),
    asserta(pending_change(Index)).

save_changes(FileContentL) :-
    index_change(Index),
    maplist(save_change(Index), FileContentL).

undo_changes(Index) :-
    retract(pending_change(Index)),
    retractall(pending_change(Index, _, _)).

reset_changes :-
    retractall(pending_change(_)),
    retractall(pending_change(_, _, _)).
