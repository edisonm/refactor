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
	  [rcommit/0,
	   rshow/0,
	   rlist/0,
	   rlist/1,
	   rdiff/0,
	   rdiff_q/0,
	   rdiff/1,
	   rdiff/2,
	   rsave/1,
	   rundo/0,
	   rdelete/1,
	   rrewind/0,
	   rrewind/1,
	   rreset/0
	  ]).

:- use_module(library(file_changes)).
:- use_module(library(ref_pending)).

rcommit :-
    once(pending_command(Index, _)),
    rdiff(save, 0, Index),
    rreset.

rlist :-
    \+ ( rlist(_),
	 fail
       ).

rlist(Index) :-
    pending_command(Index, Command),
    with_output_to(string(SCommand), portray_clause(Command)),
    print_message(information, format('Index ~w, Command: ~s', [Index, SCommand])).

rshow :-
    once(pending_command(Index, _)),
    rdiff(show, 0, Index).

rsave(Diff):-
    tell(Diff),
    rshow,
    told.

rdiff :-
    once(rdiff(_)).

rdiff_q :-
    ( current_prolog_flag(verbose, silent)
    ->true
    ; once(rdiff(Index)),
      print_message(informational, format('Saved changes in index ~w', [Index]))
    ).

rdiff(Index) :-
    pending_command(Index, _),
    succ(Index0, Index),
    rdiff(show, Index0, Index).

rdiff(Index0, Index) :-
    rdiff(show, Index0, Index).

rdiff(Action, Index0, Index) :-
    findall(File, (pending_change(IdxI, File, _), IdxI=<Index), FileU),
    sort(FileU, FileL),
    forall(member(File, FileL),
	   ( once(pending_change(_, File, Content)),
	     ( pending_change(Idx1, File, Content0 ),
	       Idx1 =< Index0
	     ->setup_call_cleanup(tmp_file_stream(text, File0, Stream),
				  ( format(Stream, '~s', [Content0 ]),
				    close(Stream),
				    do_file_change(Action, File0, File, Content)
				  ),
				  delete_file(File0))
	     ; do_file_change(Action, File, File, Content)
	     )
	   )).

rundo :-
    rundo(_).

rdelete(Index) :-
    rundo(Index),
    rrewind(Index).

rundo(Index) :-
    rdrop(Index, Command),
    print_message(information, format('Undone ~w ---> ~w', [Index, Command])).

rdrop(Index, Command) :-
    retract(pending_command(Index, Command)),
    retractall(pending_change(Index, _, _)).

rrewind :-
    rrewind(0).

rrewind(Index) :-
    findall(Command, ( pending_command(Index0, Command),
		       Index0 > Index,
		       rdrop(Index0, _)
		     ),
	    CommandR),
    reverse(CommandR, CommandL),
    maplist(apply_command, CommandL).

rreset :-
    retractall(pending_command(_, _)),
    retractall(pending_change(_, _, _)).
