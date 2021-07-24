/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2016, Process Design Center, Breda, The Netherlands.
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

:- module(pretty_decl,
          [pretty_decl/2,
           pretty_decl/3,
           pretty_decl/4]).

pretty_decl(Decl, PDecl, Id, Next) :-
    pretty_decl(Decl, PDecl, Id),
    succ(Id, Next).

pretty_decl(Decl, PDecl) :- pretty_decl(Decl, PDecl, 1).

pretty_decl((:- use_module(A, L)),
            (:- $@(use_module('$POS'('$1'(Id), A),
                              '$LISTB,NL'(L, '$1'(Id)+1)))), Id) :- !.
pretty_decl((:- module(M, L)),
            (:- $@(module('$POS'('$1'(Id), M),
                          '$LISTB,NL'(L, '$1'(Id)+1)))), Id) :- !.
pretty_decl((:- reexport(A, L)),
            (:- $@(reexport('$POS'('$1'(Id), A),
                            '$LISTB,NL'(L, '$1'(Id)+1)))), Id) :- !.
pretty_decl((:- export(L)), (:- $@(export('$NL'('$LIST,NL'(L),'$OUTPOS')))), _) :- !.
pretty_decl((:- M:export(L)), (:- $@(M:export('$NL'('$LIST,NL'(L,'$OUTPOS'),'$OUTPOS'+1)))), _) :- !.
pretty_decl(Decl, Decl, _).
