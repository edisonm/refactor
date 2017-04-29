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

:- module(i18n_file_utils,
          [current_po_file/3,
           edit_po_file/3,
           edit_po_file/2,
           edit_po_files/2,
           edit_po_files/1,
           expand_pot_files/0,
           expand_pot_file/1,   % ?Module,
           clean/3,
           sort/3,
           expand/3,
           compact/2,
           read_po_file/2,      % +PoFile, -Entries
           save_to_po_file/2,
           arrange_po_files/1,
           subtract_po_file/2]).

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(ref_changes)).
:- use_module(library(term_info)).
:- use_module(library(clambda)).
:- use_module(library(i18n/i18n_parser)).
:- use_module(library(i18n/i18n_support)).

current_po_file(M, Lang, PoFile) :-
    current_pot_file(M, PotFile),
    get_lang_file(PotFile, Lang, PoWildcard),
    expand_file_name(PoWildcard, PoFiles),
    member(PoFile, PoFiles).

read_po_file(File, Entries) :-
    ( ( pending_change(_, File, Codes)
      ->true
      ; access_file(File, read) ->
        read_file_to_codes(File, Codes, [])
      )
    ->parse_po_entries(Entries, Codes, [])
    ; Entries = []
    ).

%% subtract_po_file(PoFile1, PoFile2) is det
%
% Subtract from PoFile1 the entries that are the same in PoFile2.
subtract_po_file(PoFile1, PoFile2) :-
    read_po_file(PoFile1, Entries1),
    read_po_file(PoFile2, Entries2),
    subtract(Entries1, Entries2, Entries),
    save_to_po_file(Entries, PoFile1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commands for edit_po_file/_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clean(PoFile, ML, Codes) :-
    i18n_tmpl_entries(ML, TmplEntries),
    read_po_file(PoFile, Entries0),
    Entry = entry(_, _, Ref, _, MsgId, _),
    findall(Entry,
            ( member(Entry, Entries0),
              member(entry(_, _, Ref, _, MsgId, _), TmplEntries)
            ), Entries),
    parse_po_entries(Entries, Codes, []).

sort(PoFile, _, Codes) :-
    read_po_file(PoFile, EntriesU),
    sort(EntriesU, Entries),
    parse_po_entries(Entries, Codes, []).

%% expand(+PoFIle, ?ModuleList, -Codes) is det
%
% Creates or update a po specific language file with empty entries
% ready to be filled.
expand(PoFile, ML, Codes) :-
    read_po_file(PoFile, Entries0),
    i18n_tmpl_entries(ML, TmplEntries),
    Entry = entry(_, _, Ref, _, MsgId, _),
    findall(Entry, ( member(Entry, TmplEntries),
                     \+ member(entry(_, _, Ref, _, MsgId, _), Entries0)
                   ), Entries, Entries0),
    parse_po_entries(Entries, Codes, []).

%% compact(+PoFile,-Codes) is det
%
% Remove the empty entries in the specified .po language file.
compact(PoFile, Codes) :-
    read_po_file(PoFile, Entries0),
    findall(Entry,
            ( member(Entry, Entries0),
              Entry \= entry(_, _, _, _, _, [""])
            ), Entries),
    parse_po_entries(Entries, Codes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- meta_predicate edit_po_file(2,+,+).
edit_po_file(Command, M, Lang) :-
    findall(PoFile-Codes,
            ( current_po_file(M, Lang, PoFile),
              call(Command, PoFile, [M], Codes)
            ), FileCodes),
    save_changes(FileCodes).

:- meta_predicate edit_po_file(2,+).
edit_po_file(Command, M) :-
    edit_po_file(Command, M, '??').

:- meta_predicate edit_po_files(2,+).
edit_po_files(Command, Lang) :-
    findall(PoFile-M, current_po_file(M, Lang, PoFile), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    forall(member(PoFile-ML, Grouped),
           call(Command, PoFile, ML)).

:- meta_predicate edit_po_files(2).
edit_po_files(Command) :- edit_po_files(Command, '??').

expand_pot_file(M) :-
    findall(PotFile-Codes,
            ( current_pot_file(M, PotFile),
              i18n_tmpl_entries_module(M, EntriesU),
              sort(EntriesU, Entries),
              parse_po_entries(Entries, Codes, [])
            ),
            FileChanges),
    save_changes(FileChanges).

expand_pot_files :-
    findall(PotFile-M, current_pot_file(M, PotFile), Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    findall(PotFile-Codes,
            ( member(PotFile-ML, Grouped),
              i18n_tmpl_entries(ML, EntriesU),
              sort(EntriesU, Entries),
              parse_po_entries(Entries, Codes, [])
            ),
            FileChanges),
    save_changes(FileChanges).

read_entries(FL, Lang, TEntriesL) :-
    findall(PotFile, current_pot_file(_, PotFile), FL0),
    sort(FL0, FL1),
    read_entries(FL1, [], FL, Lang, TEntriesL).

read_time_entries(PotFile, Lang, Time-Entries) :-
    get_lang_file(PotFile, Lang, PoFile),
    read_po_file(PoFile, Entries),
    Entries \= [],
    time_file(PoFile, Time).

read_entries([],  FL,  FL, _,    []) :- !.
read_entries(FD0, FL0, FL, Lang, TEntriesL) :-
    findall(TEntries,
            ( member(PotFile, FD0),
              read_time_entries(PotFile, Lang, TEntries)
            ),
            TEntriesL, TEntriesT),
    append(FL0, FD0, FL1),
    findall(PotFile, ( TEntriesT = [], % Temporarily close the list
                       member(_-Entries, TEntriesL),
                       member(Entry, Entries),
                       determine_module(Entry, M),
                       current_pot_file(M, PotFile),
                       \+ memberchk(PotFile, FL1)
               ), FD1),
    sort(FD1, FD2),
    read_entries(FD2, FL1, FL, Lang, TEntriesT).

% Note: if the key is the same, merge will take the entry from the
% newer file
merge_entries_list([],                        TEntries, TEntries).
merge_entries_list([Time-EntriesL|TEntriesL], TEntries0, TEntries) :-
    merge_entries(EntriesL, Time, TEntries0, TEntries1),
    merge_entries_list(TEntriesL, TEntries1, TEntries).

merge_entries([],               _, TEntries, TEntries).
merge_entries([Entry0|Entries], Time0, TEntries0, TEntries) :-
    Entry0 = entry(_, _, Ref, _, MsgId, _),
    ( select(Time1-entry(_, _, Ref, _, MsgId, _), TEntries0, TEntries1)
    ->                          % Two entries with same Key
      ( Time1 < Time0
      ->TEntries2 = [Time0-Entry0|TEntries1] % Entry1 will be replaced
      ; TEntries2 = TEntries0                % Entry1 will be preserved
      )
    ; TEntries2 = [Time0-Entry0|TEntries0]
    ),
    merge_entries(Entries, Time0, TEntries2, TEntries).

save_to_po_file(Entries, PoFile) :-
    parse_po_entries(Entries, Codes, []),
    save_changes([PoFile-Codes]).

arrange_po_files(Lang) :-
    read_entries(FL, Lang, TEntriesL),
    merge_entries_list(TEntriesL, [], TEntries),
    pairs_values(TEntries, Entries),
    maplist(\Entry^(F-Entry)^ ( determine_module(Entry, M),
                                once(current_pot_file(M, F))
                              ),
            Entries, UFEntries),
    keysort(UFEntries, FEntries),
    group_pairs_by_key(FEntries, GFEntries),
    pairs_keys(GFEntries, UF),
    subtract(FL, UF, EFL),
    maplist(\F^(F-[])^true, EFL, EFEntries),
    append(GFEntries, EFEntries, AFEntries),
    maplist([Lang] +\ (F-UE)^(PoFile-Codes)^
           ( sort(UE, E),
             get_lang_file(F, Lang, PoFile),
             parse_po_entries(E, Codes, [])
           ), AFEntries, FileChanges),
    save_changes(FileChanges).

determine_module(entry(_, _, Ref, _, _, _), M) :-
    reference(M, Ref).

% Template utilities:

:- dynamic i18n_po_tmpl/2.

assert_entry_tmpl((~), M, MsgId, [""]) :- !,
    reference(M, Ref),
    Entry = entry([], [], Ref, [], MsgId, [""]),
    ( i18n_po_tmpl(Entry, M) -> true
    ; assertz(i18n_po_tmpl(Entry, M))
    ).
assert_entry_tmpl((~~), _, _, [""]). % Just ignore by now

collect_i18n_term(M, (:- resourceterm(Term))) :-
    i18n_process_term(assert_entry_tmpl((~)), M, Term, _),
    !.
collect_i18n_term(M, Term) :-
    expand_i18n_term(assert_entry_tmpl, M, Term, _).

i18n_tmpl_entries(ML, Entries) :-
    collect_i18n_entries_by_module,
    findall(Entry,
            ( member(M, ML),
              retract(i18n_po_tmpl(Entry, M))
            ), Entries).

i18n_tmpl_entries_module(M, Entries) :-
    collect_i18n_entries_by_module,
    findall(Entry, retract(i18n_po_tmpl(Entry, M)), Entries).

get_term_info(M, Pattern, Options) :-
    get_term_info(M, Pattern, Term, \_^true, _, Options),
    Term = Pattern.

collect_i18n_entries_by_module :-
    retractall(i18n_po_tmpl(_, _)),
    ( i18n_support:current_i18n_module(M),
      get_term_info(M, RawTerm, []),
      collect_i18n_term(M, RawTerm),
      fail
    ; true
    ).
