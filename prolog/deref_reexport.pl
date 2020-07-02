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

:- module(deref_reexport,
          [deref_reexport/2,
           deref_reexport/3
          ]).

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(called_from)).
:- use_module(library(from_utils)).
:- use_module(library(infer_alias)).
:- use_module(library(pretty_decl)).
:- use_module(library(file_to_module)).

deref_reexport(Alias, Options) :-
    deref_reexport(Alias, _, Options).

deref_reexport(Alias, Reexported, Options) :-
    absolute_file_name(Alias, AFile, [file_type(prolog), access(read)]),
    module_property(M, file(AFile)),
    module_property(M, exports(ExL)),
    ( \+ ( member(F/A, ExL),
           functor(H, F, A),
           predicate_property(M:H, imported_from(_))
         )
    ->print_message(information, format("~w does not have reexports", [Alias]))
    ; freeze(H1, once((member(F/A, ExL), functor(H1, F, A)))),
      collect_called_from(H1, Reexported, _, _, Options),
      findall(File/CM, called_from_w(_, M, Reexported, CM, File), FileCMU),
      sort(FileCMU, FileCML),
      findall(File/CM-RMPIG,
              ( member(File/CM, FileCML),
                findall((Reexported-F/A),
                        ( called_from_w(H2, M, Reexported, CM, File),
                          functor(H2, F, A),
                          ( Reexported = M
                          ->true
                          ; \+ declared_use_module(F, A, Reexported, CM, _, File)
                          )
                        ), RMPIU),
                sort(RMPIU, RMPIL),
                group_pairs_by_key(RMPIL, RMPIG)
              ), FileRMPIG),
      forall(member(File/CM-RMPIL, FileRMPIG),
             update_use_module(AFile, M, RMPIL, File, CM))
    ).

called_from_w(H, M, RM, CM, File) :-
    called_from:called_from_db(H, RM, CM, _, From),
    RM \= CM,
    ( RM = M
    ->true
    ; predicate_property(M:H, imported_from(RM))
    ),
    from_to_file(From, File).

update_use_module(AFile, M, RMPIL, File, CM) :-
    module_property(M, exports(ExL)),
    replace_sentence((:- use_module(A)),
                     DeclL,
                     collect_decls(AFile, File, RMPIL, CM, A, ExL, ExL, DeclL),
                     [file(File)]),
    replace_sentence((:- use_module(A, ImS)),
                     DeclL,
                     collect_decls(AFile, File, RMPIL, CM, A, ExL, ImS, DeclL),
                     [file(File)]).

collect_decls(AFile, File, RMPIL, CM, A, ExL, ImS, DeclL) :-
    absolute_file_name(A, AF, [file_type(prolog), access(read), relative_to(File)]),
    AF = AFile,
    ( ImS = except(Exc)
    ->subtract(ExL, Exc, ImL)
    ; ImL = ImS
    ),
    ImL \= [],
    findall(PDecl,
            ( member(RM-RPIL, RMPIL),
              intersection(RPIL, ImL, PIL),
              module_property(RM, file(RF)),
              library_alias(RF, RA),
              ( \+ ( module_property(RM, exports(ExL)),
                     member(F/A, ExL),
                     \+ member(F/A, PIL),
                     functor(H, F, A),
                     module_property(CM:H, defined)
                   )
              ->Decl = (:- use_module(RA))
              ; Decl = (:- use_module(RA, PIL))
              ),
              pretty_decl(Decl, PDecl)
            ), DeclL).
