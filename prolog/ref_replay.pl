/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
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
          [rcommit/0,
           rlist/0,
           rlist/1,
           rrewind/0,
           rrewind/1,
           rreset/0,
           rundo/0,
           rdelete/1,
           rstats/0,
           rnostats/0,
           apply_command_q/1
          ]).

:- use_module(library(ref_shell)).
:- use_module(library(ref_changes),
              [reset_changes/0,
               pending_change/1,
               undo_changes/1]).
:- use_module(library(ref_msgtype)).
:- use_module(library(ref_command)).

:- meta_predicate apply_command_q(0).
apply_command_q(Call) :-
    apply_command(Call),
    once(pending_change(Index)),
    message_type(Type),
    print_message(Type, format('Saved changes in index ~w', [Index])).

rstats :-
    retractall(rstats_db),
    assertz(rstats_db).

rnostats :-
    retractall(rstats_db).

rcommit :-
    ref_commit,
    reset_commands.

rlist :-
    \+ ( rlist(_),
         fail
       ).

rlist(Index) :-
    pending_command(Index, Command),
    with_output_to(string(SCommand), portray_clause(Command)),
    print_message(information, format('Index ~w, Command: ~s', [Index, SCommand])).

rrewind :-
    rrewind(0).

rrewind(Index) :-
    findall(Command, ( pending_command(Index1, Command),
                       Index1 > Index,
                       rdrop(Index1, _)
                     ),
            CommandR),
    reverse(CommandR, CommandL),
    maplist(apply_command, CommandL).

rundo :-
    rundo(_).

rdelete(Index) :-
    rundo(Index),
    rrewind(Index).

rundo(Index) :-
    rdrop(Index, Command),
    print_message(information, format('Undone ~w ---> ~w', [Index, Command])).

rdrop(Index, Command) :-
    undo_changes(Index),
    undo_command(Index, Command).

rreset :-
    reset_changes,
    reset_commands.
