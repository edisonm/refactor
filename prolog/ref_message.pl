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

:- module(ref_message,
          [refactor_message/2,
           textpos_line/3
          ]).

:- use_module(library(codesio)).
:- use_module(library(ref_context)).
:- use_module(library(seek_text)).

%!  refactor_message(+Type, +Message) is det.
%
%   Print a message but first showing the location of the source code being
%   refactorized. Intended to be used in the expander of a refactoring call.
%
refactor_message(Type, Message) :-
    refactor_location(From),
    print_message(Type, at_location(From, Message)).

refactor_location(From) :-
    refactor_context(file, File),
    ( refactor_context(termpos, TermPos),
      TermPos \= none
    ->( refactor_context(text, Text)
      ->arg(1, TermPos, CharPos),
        textpos_line(Text, CharPos, Line, Pos),
        From = file(File, Line, Pos, _)
      ; From = file_term_position(File, TermPos)
      )
    ; refactor_context(pos, CharPos),
      stream_position_data(line_count, CharPos, Line),
      From = file(File, Line, -1, _)
    ).

textpos_line(Text, CharPos, Line, LinePos) :-
    setup_call_cleanup(
        ( open_codes_stream(Text, In),
          open_null_stream(Out)
        ),
        ( copy_stream_data(In, Out, CharPos),
          stream_property(In, position(Pos)),
          stream_position_data(line_count, Pos, Line),
          stream_position_data(line_position, Pos, LinePos)
        ),
        ( close(Out),
          close(In)
        )).

textpos_line(Text, CharPos, LinePos) :-
    ( seek1_char_left(Text, '\n', CharPos, From)
    ->LinePos is CharPos - (From + 1)
    ; LinePos is CharPos
    ).

/* This was too slow --EMM
textpos_line(Text, CharPos, LinePos) :-
    setup_call_cleanup(
        ( open_codes_stream(Text, In),
          open_null_stream(Out)
        ),
        ( copy_stream_data(In, Out, CharPos),
          stream_property(In, position(Pos)),
          stream_position_data(line_position, Pos, LinePos)
        ),
        ( close(Out),
          close(In)
        )).
*/
