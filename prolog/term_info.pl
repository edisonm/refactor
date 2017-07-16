/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
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

:- module(term_info,
          [ get_term_info/6,
            with_source_file/3,
            fetch_term_info/4
          ]).

:- use_module(library(apply)).
:- use_module(library(prolog_source)).
:- use_module(library(clambda)).
:- use_module(library(module_files)).

:- meta_predicate
    with_source_file(+, -, 0),
    get_term_info(+, ?, ?, 1, -, +),
    transverse_apply_2(1, +, ?, ?, ?).

get_term_info(M, Pattern, Term, AllChk, File, Options) :-
    module_files(M, Files),
    member(File, Files),
    call(AllChk, File),
    get_term_info_file(Pattern, Term, File, Options).

fix_exception(error(Error, stream(_,  Line, Row, Pos)), File,
              error(Error, file(File, Line, Row, Pos))) :- !.
fix_exception(E, _, E).

ti_open_source(Path, In) :-
    b_setval(ti_open_source, yes),
    prolog_open_source(Path, In),
    b_setval(ti_open_source, no).

with_source_file(File, In, Goal) :-
    prolog_canonical_source(File, Path),
    print_message(informational, format("Reading ~w", Path)),
    catch(setup_call_cleanup(ti_open_source(Path, In),
                             call(Goal),
                             prolog_close_source(In)),
          E0,
          ( fix_exception(E0, Path, E),
            print_message(error, E),
            fail
          )).

fetch_term_info(Pattern, Term, Options, In) :-
    ( ( option(line(Line), Options),
        nonvar(Line)
      ->seek(In, 0, bof, _),
        prolog_source:seek_to_line(In, Line),
        once(get_term_info_fd(In, Pattern, Term, Options))
      ; get_term_info_fd(In, Pattern, Term, Options)
      )
    ; fail                      % don't close In up to the next iteration
    ).

get_term_info_file(Pattern, Term, File, Options) :-
    with_source_file(File, In, fetch_term_info(Pattern, Term, Options, In)).

get_term_info_fd(In, PatternL, TermL, OptionL0 ) :-
    is_list(PatternL), !,
    foldl(\ (H-D)^O0^O^select_option(H, O0, O, D),
                [subterm_positions(TermPos)-TermPos,
                 comments(Comments)-Comments,
                 variable_names(VN)-VN
                ], OptionL0, OptionT),
    PatternL = [_|PatternT],
    length(PatternT, TN),
    length(TA, TN),
    maplist([In, OptionT] +\
           T^P^V^C^get_term_info_each(In, OptionT, [T, P, V, C]), TA, PA, VA, CA),
    maplist(append, [TA, PA, VA, CA], TPVCT, TPVCH),
    transverse_apply_2(get_term_info_each(In, OptionT), TPVCH, TPVCT,
                       [TermL, TermPosL, VNL, CommentsL], [_, TermPosE, _, _]),
    maplist(subsumes_term, PatternL, TermL),
    append(CommentsL, Comments),
    append(VNL, VN),
    TermPosL = [TermPosI|_],
    arg(2, TermPosE, To),
    findall(From, ( Comments = [StreamPos-_|_],
                    stream_position_data(char_count, StreamPos, From)
                  ; arg(1, TermPosI,  From)
                  ),
            FromL),
    min_list(FromL, From),
    TermPos = list_position(From, To, TermPosL, none).
get_term_info_fd(In, Pattern, Term, Options0 ) :-
    should_set_line(SetLine, Options0, Options),
    repeat,
      '$set_source_module'(M, M),
      read_term(In, Term, [module(M)|Options]),
      ( Term == end_of_file
      ->!,
        fail
      ; true
      ),
      set_line(SetLine),
      ( member(ModDecl, [(:- module(CM, _)), (:- module(CM, _, _))]),
        subsumes_term(ModDecl, Term),
        Term = ModDecl
      ->'$set_source_module'(_, CM)
      ; true
      ),
      subsumes_term(Pattern, Term).

should_set_line(posline(Pos, Line), Options, [term_position(Pos)|Options]) :-
    option(line(Line), Options), !.
should_set_line(no, Options, Options).

set_line(no).
set_line(posline(Pos, Line)) :-
    stream_position_data(line_count, Pos, Line).

transverse_apply(_,     ListL,  ListT, ListL, EL, EL) :- maplist(=([]), ListT).
transverse_apply(Apply, ListH0, ListT, ListL, _,  EL) :-
    maplist(\ [_|L]^L^true, ListH0, ListH),
    transverse_apply_2(Apply, ListH, ListT, ListL, EL).

transverse_apply_2(Apply, ListH, ListT0, ListL, EL) :-
    call(Apply, EL0 ),
    maplist(\ E^[E|L]^L^true, EL0, ListT0, ListT),
    transverse_apply(Apply, ListH, ListT, ListL, EL0, EL).

get_term_info_each(In, Options, [T, P, V, C]) :-
    '$set_source_module'(M, M),
    read_term(In, T, [subterm_positions(P), variable_names(V), comments(C), module(M)|Options]),
    T \== end_of_file.

:- public read_terms/3.

read_terms(In, TermOptsL, Options) :-
    read_terms_opts(TermOptsL, In, Options, TermOptsL0, TermOptsT),
    read_terms_opts_rec(In, Options, TermOptsL0, TermOptsT, TermOptsL).

read_terms_opts([],    _,  _,         TermOptsT,           TermOptsT).
read_terms_opts([_|T], In, Options0, [TermOpts|TermOptsL], TermOptsT) :-
    read_term_opts(In, Options0, TermOpts),
    read_terms_opts(T, In, Options0, TermOptsL, TermOptsT).

read_term_opts(In, Options0, Term-Options) :-
    copy_term(Options0, Options),
    read_term(In, Term, Options).

read_terms_opts_rec(_,  _,        TermOptsL,       [],                   TermOptsL).
read_terms_opts_rec(In, Options0, [_|TermOptsL0 ], [TermOpts|TermOptsT], TermOptsL) :-
    read_term_opts(In, Options0, TermOpts),
    read_terms_opts_rec(In, Options0, TermOptsL0, TermOptsT, TermOptsL).
