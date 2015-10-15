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

:- module(ref_shell,
	  [ref_commit/0,
	   rshow/0,
	   rdiff/0,
	   rdiff/1,
	   rdiff/2,
	   rsave/1
	  ]).

:- use_module(refactor(file_changes)).
:- use_module(refactor(ref_changes)).

ref_commit :-
    once(pending_change(Index)),
    rdiff(save, 0, Index),
    reset_changes.

rshow :-
    once(pending_change(Index)),
    rdiff(show, 0, Index).

rsave(Diff):-
    tell(Diff),
    rshow,
    told.

rdiff :-
    once(rdiff(_)).

rdiff(Index) :-
    pending_change(Index),
    succ(Index0, Index),
    rdiff(show, Index0, Index).

rdiff(Index0, Index) :-
    rdiff(show, Index0, Index).

rdiff(Action, Index0, Index) :-
    findall(File, (pending_change(IdxI, File, _), IdxI=<Index), FileU),
    sort(FileU, FileL),
    forall(member(File, FileL),
	   apply_diff(Action, Index0, File)).

apply_diff(Action, Index0, File) :-
    once(pending_change(_, File, Content)), % Take the last one
    ( pending_change(Idx1, File, Content0 ),
      Idx1 =< Index0
    ->setup_call_cleanup(tmp_file_stream(text, File0, Stream),
			 format(Stream, '~s', [Content0 ]),
			 close(Stream)),
      TmpFile = true
    ; read_file_to_string(File, Content0, []),
      File0 = File,
      TmpFile = fail
    ),
    ( Content0 \= Content
    ->do_file_change(Action, File0, File, Content)
    ; true
    ),
    ( TmpFile = true
    ->delete_file(File0 )
    ; true
    ).
