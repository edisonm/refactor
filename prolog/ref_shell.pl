/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(ref_shell,
          [ rcommit/0,
            rdiff/0,
            rdiff/1,
            rdiff/2,
            rreset/0,
            rsave/1,
            rshow/0
          ]).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(trim_utils)).
:- use_module(library(file_changes)).
:- use_module(library(ref_changes)).
:- use_module(library(ref_command)).

/** <module> Pending changes management

   This module implements the interactive command‑line interface used by the
   refactoring library.  It provides predicates to inspect, save, diff, and
   finally commit the stack of pending changes that are produced by the
   `replace/5` family of predicates.

   Typical workflow:

   1. Run a refactoring, e.g.
      ```prolog
      replace(sent, (:-use_module(X)), _, _, [file(F)])
      ```
   2. Inspect the pending modifications with `rshow/0` or `rdiff/0`.
   3. Optionally write the diff to a file using `rsave/1`.
   4. Apply the changes permanently with `rcommit/0`.
   5. Reset the session with `rreset/0` to start a new refactoring.

   The predicates are thin wrappers around the change stack maintained by
   `library(ref_changes)`.  All options that control the behaviour of the
   refactoring (e.g., `file/1`, `max_changes/1`, `linearize/1`) are passed to
   `replace/5`; the shell itself does not introduce additional options.

   @see replace/5 for the full list of supported options.
*/

ref_commit :-
    once(pending_change(Index)),
    rdiff(save, 0, Index),
    reset_changes.

%!  rshow
%   Show the list of pending changes.  If no change is pending the predicate fails.
%   The output is a formatted diff printed to the current output stream.
rshow :-
    once(pending_change(Index)),
    rdiff(show, 0, Index).

%!  rsave(+Diff)
%   Save the current diff (the pending changes) into the file named Diff.
%   The predicate opens Diff for writing, prints the formatted diff using `rshow/0`,
%   and then closes the file.
%   It is a convenience wrapper around `tell/1`/`told/0`.
rsave(Diff) :-
    tell(Diff),
    rshow,
    told.

%!  rdiff
%   Show the diff of the most recent pending change.  This predicate is a
%   convenience wrapper that calls `rdiff/1` with the index of the latest
%   change.
rdiff :-
    once(rdiff(_)).

%!  rdiff(+Index)
%   Show the diff for the pending change identified by Index.  Index is the
%   numeric identifier of a change as returned by `pending_change/1`.  The
%   predicate prints a diff that includes all changes up to and including the
%   specified Index.
rdiff(Index) :-
    pending_change(Index),
    succ(Index1, Index),
    rdiff(show, Index1, Index).

%!  rdiff(+Index1, +Index)
%   Low‑level helper used by `rdiff/0`, `rdiff/1` and `rdiff/2`.  It prints the
%   diff for all pending changes whose indices lie between `Index1` (exclusive)
%   and `Index` (inclusive).  Normally callers compute `Index1` as `Index-1` to
%   obtain the diff of a single change.
rdiff(Index1, Index) :-
    rdiff(show, Index1, Index).

rdiff(Action, Index1, Index) :-
    findall(File, (pending_change(IdxI, File, _), IdxI=<Index), FileU),
    sort(FileU, FileL),
    forall(member(File, FileL),
           apply_diff(Action, Index1, File)).

trim_content(RawContent, Content) :-
    atomics_to_string(RawList, "\n", RawContent),
    maplist(string_right_trim, RawList, List),
    atomics_to_string(List, "\n", Content).

apply_diff(Action, Index1, File) :-
    once(pending_change(_, File, RawContent)), % Take the last one
    trim_content(RawContent, Content), % Remove right spaces
    ( pending_change(Idx1, File, Content1),
      Idx1 =< Index1
    ->setup_call_cleanup(tmp_file_stream(text, File1, Stream),
                         format(Stream, '~s', [Content1]),
                         close(Stream)),
      TmpFile = true
    ; File1 = File,
      TmpFile = fail,
      ( access_file(File, read)
      ->read_file_to_string(File, Content1, [])
      ; Content1 = []
      )
    ),
    ( Content1 \= Content
    ->do_file_change(Action, File1, File, Content)
    ; true
    ),
    ( TmpFile = true
    ->delete_file(File1)
    ; true
    ).

%!  rcommit
%   Apply all pending changes to their respective source files and clear the
%   change stack.  This is equivalent to calling `ref_commit/0` followed by
%   `reset_commands/0`.
%   @see rreset/0 to discard pending changes without applying them.
rcommit :-
    ref_commit,
    reset_commands.

%!  rreset
%   Discard all pending changes and reset the command database.  The
%   refactoring session returns to a clean state as if no `replace/5`
%   operations had been performed.
%   Use this after inspecting changes with `rshow/0` when you decide not to
%   apply them.
rreset :-
    reset_changes,
    reset_commands.
