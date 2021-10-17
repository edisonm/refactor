:- module(facts_to_table,
          [ facts_to_table/2,
            wd_alias2/3
          ]).

:- meta_predicate facts_to_table(0, +).

facts_to_table(MFact, Table) :-
    MFact = _:Fact,
    tell(Table),
    Fact =.. [_, Arg1|Args],
    setup_call_cleanup(
        open(Table, write, Stream, [encoding(utf8)]),
        with_output_to(
            Stream,
            forall(MFact,
                   ( writeq(Arg1),
                     forall(member(Arg, Args),
                            ( write('\t'),
                              write(Arg)
                            )),
                     nl
                   ))),
        close(Stream)).

:- dynamic stored_idtable_handle/1.

idtable(Handle) :-
    stored_idtable_handle(Handle),
    !.
idtable(Handle) :-
    new_table('wd_alias.tab',
              [id(integer), lang(atom), name(atom)],
              [field_separator(0'\t), encoding(native)], Handle),
    assert(stored_idtable_handle(Handle)).

wd_alias2(Id, Lang, Name) :-
    idtable(Handle),
    in_table(Handle, [id(Id), lang(Lang), name(Name)], _).

/*
:- use_module('/home/edison/tmp/wd_alias').

wd_alias_to_table(Table) :-
    tell(Table),
    aggregate_all(t(max(LId), max(LLang), max(LName)),
                  ( wd_alias(Id, Lang, Name),
                    write_length(Id,   LId,   []),
                    write_length(Lang, LLang, []),
                    write_length(Name, LName, [])
                  ), t(LId, LLang, LName)),
    Tab1 is LId,
    Tab2 is LId  + LLang,
    % Tab3 is Tab2 + LName,
    forall(wd_alias(Id, Lang, Name),
           ( % format("~d~` t~*|~s~` t~*|~s~` t~*|~n", [Id, Tab1, Lang, Tab2, Name, Tab3])
               format("~d~` t~*|~s~` t~*|~s~n", [Id, Tab1, Lang, Tab2, Name])
           )),
    told.
*/
