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
          [ref_commit/0,
           rshow/0,
           rdiff/0,
           rdiff/1,
           rdiff/2,
           rsave/1
          ]).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(trim_utils)).
:- use_module(library(file_changes)).
:- use_module(library(ref_changes)).

ref_commit :-
    once(pending_change(Index)),
    rdiff(save, 0, Index),
    reset_changes.

rshow :-
    once(pending_change(Index)),
    rdiff(show, 0, Index).

rsave(Diff) :-
    tell(Diff),
    rshow,
    told.

rdiff :-
    once(rdiff(_)).

rdiff(Index) :-
    pending_change(Index),
    succ(Index1, Index),
    rdiff(show, Index1, Index).

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
