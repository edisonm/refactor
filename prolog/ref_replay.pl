/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(ref_replay,
          [ apply_command_q/1,
            rdelete/1,
            rlist/0,
            rlist/1,
            rnostats/0,
            rrewind/0,
            rrewind/1,
            rstats/0,
            rundo/0
          ]).

:- use_module(library(lists)).
:- use_module(library(listing)).
:- use_module(library(ref_changes),
              [ pending_change/1,
                undo_changes/1]).
:- use_module(library(ref_msgtype)).
:- use_module(library(ref_command)).

/** <module> Individual pending changes management

  This library  provides tools  to manage  individual elements  of the  stack of
  pending changes of the refactoring tool.  The pending changes are indexed by a
  sequential number, so that you can use such index in such predicates.

*/

:- meta_predicate apply_command_q(0).

%!  apply_command_q(:Call).

%   Execute Call and  associated such call with an index  for further management
%   of such Call.

apply_command_q(Call) :-
    apply_command(Call),
    once(pending_change(Index)),
    message_type(Type),
    print_message(Type, format('Saved changes in index ~w', [Index])).

%!  rstats is det.
%
%   Stablish that it must  be printed an information message at at  the end of a
%   command.

rstats :-
    retractall(rstats_db),
    assertz(rstats_db).

%!  rnostats is det.
%
%   Stablish that it must be printed an informational message at at the end of a
%   command, this means that only when the verbosity is the highest you will see
%   that message.

rnostats :-
    retractall(rstats_db).

%!  rlist is det.
%
%   Display all commands that are waiting to be applied.

rlist :-
    \+ ( rlist(_),
         fail
       ).

%!  rlist(Index) is det.
%
%   Display on  backtracking the commands  that are  waiting to be  applied, and
%   unifies Index with such command's index.

rlist(Index) :-
    pending_command(Index, Command),
    with_output_to(string(SCommand), portray_clause(Command)),
    print_message(information, format('Index ~w, Command: ~s', [Index, SCommand])).

%!  rrewind is det.
%
%   Reaply all the pending commands.  This is useful if you change a file before
%   to commit the  changes, making the refactoring outdated with  respect to the
%   current files.

rrewind :-
    rrewind(0).

%!  rrewind(Index) is det.
%
%   Reaply all the commands whose index is greater than Index.

rrewind(Index) :-
    findall(Command, ( pending_command(Index1, Command),
                       Index1 > Index,
                       rdrop(Index1, _)
                     ),
            CommandR),
    reverse(CommandR, CommandL),
    maplist(apply_command, CommandL).

%!  rundo is multi.
%
%   Undo refactoring commands on backtracking.

rundo :-
    rundo(_).

%!  rdelete(Index).
%
%   Undo and rewind refactoring commands on backtracking.

rdelete(Index) :-
    rundo(Index),
    rrewind(Index).

%!  rundo(-Index) is multi.
%
%   Undo refactoring commands on backtracking and unifies Index with the index
%   of the removed command.

rundo(Index) :-
    rdrop(Index, Command),
    print_message(information, format('Undone ~w ---> ~w', [Index, Command])).

%!  rdrop(-Index) is multi.
%
%   Undo refactoring  commands on backtracking,  unifies Index and  Command with
%   the index and the removed command respectively.

rdrop(Index, Command) :-
    undo_changes(Index),
    undo_command(Index, Command).
