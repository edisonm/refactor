:- module(i18n_trans, [translate/4,
                       translate/5,
                       translate_po/4,
                       rename_id/4,
                       replace_entry_po/4,
                       i18n_refactor/4]).

:- use_module(library(ref_changes)).
:- use_module(library(ref_scenarios)).
:- use_module(library(i18n/i18n_file_utils)).
:- use_module(library(i18n/i18n_parser)).
:- use_module(library(i18n/i18n_support)).

% :- pred translate(+,+,?,?,+).
translate(M, Lang, TermEngl, TermLang, OptionL) :-
    translate_po(M, Lang, TermEngl, TermLang),
    replace_term(TermLang, TermEngl, [module(M)|OptionL]).

translate(M, Lang, EnglLangL, OptionL) :-
    forall(member(Engl-Lang, EnglLangL),
           translate_po(M, Lang, Engl, Lang)),
    replace_term(TermLang, TermEngl,
                 ( member(TermEngl-Lang, EnglLangL),
                   TermLang=@=Lang,
                   TermLang=Lang
                 ), [module(M)|OptionL]).

translate_po(M, Lang, TermEngl, TermLang) :-
    i18n_refactor(update_po_entry(Lang), M, TermLang, TermEngl).

%% rename_id(?Module, +OldTerm, +NewTerm, +Action) is det
%
% Change an English Term by another and update its key in the po
% files.
rename_id(M, OldTerm, NewTerm, OptionL) :-
    i18n_refactor(rename_id_lang, M, OldTerm, NewTerm),
    replace_term(OldTerm, NewTerm, [module(M)|OptionL]).

% TODO: optimize
:- meta_predicate replace_entry_po(+,+,+,2).
replace_entry_po(M, MsgId0, MsgId, StrReplacer) :-
    freeze(MsgStr0, call(StrReplacer, MsgStr0, MsgStr)),
    findall(PoFile-Codes,
            ( current_po_file(M, '??', PoFile),
              read_po_file(PoFile, Entries0),
              replace_entry(M, MsgId0, MsgStr0, MsgId, MsgStr, Entries0, Entries),
              parse_po_entries(Entries, Codes, [])
            ), FileCodes),
    save_changes(FileCodes).

:- meta_predicate i18n_refactor(3,+,+,+).

i18n_refactor(PoUpdater, M, Term0, Term) :-
    i18n_to_translate(Term0, _, KeysValues, []),
    i18n_to_translate(Term,  _, ValuesKeys, []),
    pairs_keys_values(KeysValues, MsgId0, MsgId),
    pairs_keys_values(ValuesKeys, MsgId,  MsgId0),
    call(PoUpdater, M, MsgId0, MsgId).

update_po_entry(Lang, M, MsgStr, MsgId) :-
    update_po_entry(Lang, M, MsgStr, MsgId, MsgStr).

update_po_entry(Lang, M, MsgId0, MsgId, MsgStr) :-
    current_po_file(M, Lang, PoFile),
    read_po_file(PoFile, Entries0),
    replace_entry(M, MsgId0, _, MsgId, MsgStr, Entries0, Entries),
    save_to_po_file(Entries, PoFile).

rename_id_lang(MsgId0, MsgId, M) :-
    findall(PoFile-Codes,
            ( current_po_file(M, '??', PoFile),
              rename_id_po_file(PoFile, MsgId0, MsgId, M, Codes)
            ), FileCodes),
    save_changes(FileCodes).

rename_id_po_file(PoFile, MsgId0, MsgId, M, Codes) :-
    read_po_file(PoFile, Entries0),
    replace_entry(M, MsgId0, MsgStr, MsgId, MsgStr, Entries0, Entries),
    parse_po_entries(Entries, Codes, []).

replace_entry(M, MsgId0, MsgStr0, MsgId, MsgStr, Entries0, Entries) :-
    reference(M, Ref),
    ( member(Entry0, Entries0),
      Entry0 = entry(_, _, Ref, _, MsgId0, MsgStr0) ->
      setarg(5, Entry0, MsgId),
      setarg(6, Entry0, MsgStr),
      Entries = Entries0
    ; MsgId0 \== MsgId,
      member(Entry, Entries0),
      Entry =  entry(_, _, Ref, _, MsgId, MsgStr1) ->
      ( MsgStr = MsgStr1 -> true
      ; setarg(6, Entry, MsgStr)
      ),
      Entries = Entries0
    ; nonvar(MsgStr) ->
      Entry =  entry([], [], Ref, [], MsgId, MsgStr),
      append(Entries0, [Entry], Entries)
    ; Entries = Entries0
    ).
