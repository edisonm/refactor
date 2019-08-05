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

:- module(ref_replace,
          [replace/5,
           refactor_message/2,
           refactor_context/2,
           with_termpos/2,
           op(100,xfy,($@)),
           op(100,xfy,(@@))
          ]).

/** <module> Basic Term Expansion operations

  This library provides the predicate replace/5, which is the basic entry point
  for all the refactoring scenarios.

  Note for implementors/hackers:
  Be careful with some variables, they uses destructive assignment --TODO:
  document them

*/

:- use_module(library(apply)).
:- use_module(library(codesio)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(context_values)).
:- use_module(library(foldnl)).
:- use_module(library(term_size)).
:- use_module(library(prolog_source), []). % expand/4
:- use_module(library(readutil)).
:- use_module(library(fix_termpos)).
:- use_module(library(mapnargs)).
:- use_module(library(gcb)).
:- use_module(library(ref_changes)).
:- use_module(library(ref_msgtype)).
:- use_module(library(term_info)).
:- use_module(library(clambda)).
:- use_module(library(mapnlist)).
:- use_module(library(mklinear)).
:- use_module(library(substitute)).
:- use_module(library(subpos_utils)).
:- use_module(library(option_utils)).

:- thread_local
    command_db/1.

:- multifile
    prolog:xref_open_source/2.  % +SourceId, -Stream

:- dynamic
    rportray_pos/2,
    ref_position/3,
    rportray_skip/0.

:- meta_predicate
    apply_commands(?, +, +, ?, +, +, +, 4),
    fixpoint_file(+, +, 0),
    refactor_context(?, ?),
    replace(+,?,?,0,:),
    rportray_list(+, 2, +, +),
    with_context(?, ?, ?, ?, ?, ?, ?, 0),
    with_counters(0, +),
    with_from(0, ?),
    with_termpos(0, ?),
    with_pattern_into(0, ?, ?),
    with_styles(0, +).

%!  replace(+Level, +Term, -Into, :Expander, :Options) is det
%
%   Given a Level of operation, in all terms of the source code that subsumes
%   Term, replace each Term with the term Into, provided that the goal Expander
%   succeeds.  Expander can be used to finalize the shape of Into as well as to
%   veto the expansion (if fails). The Options argument is used to control the
%   behavior and scope of the predicate.
%
%   The predicate is efficient enough to be used also as a walker to capture all
%   matches of Term, and failing to avoid the replacement. For example:
%
%     ```
%     replace(
%         sent,
%         (:-use_module(X)), _,
%         (refactor_message(information, format("~w", [X])), fail),
%         [file(F)])
%     ```
%
%   will display all the occurrences of use_module/1 declarations in the file
%   F. Would be useful for some complex refactoring scenarios.
%
%   The levels of operations stablishes where to look for matching terms, and
%   could take one of the following values:
%
%   * goal
%     Look for terms that match a given goal.  This is implemented using the source
%     reader
%
%   * term
%     Look for sub-terms in a given read term recursivelly.
%
%   * sent
%     Look for a matching term
%
%   * head
%     Look for matching clause heads
%
%   * head_rec
%     In a clause head, look for matching terms recursivelly
%
%   * body
%     Look for a matching clause body
%
%   * body_rec
%     In a clause body, look for matching terms recursivelly
%
%
%   If level is sent, some special cases of Term are used to control its
%   behavior:
%
%   * []
%     Adds an extra sentence at the top of the file.
%
%   * end_of_file
%     Adds an extra sentence at the bottom of the file.
%
%   * [_|_]
%     Replace list of sentences
%
%   * '$NODOT'(X)
%     Print X but without the ending dot
%
%   The term Into could contain certain hacks to control its behavior, as
%   follows:
%
%   * X @@ Y
%     Print the term X with the surroundings of Y (comments, etc.).  This is
%     useful to preserve comments in Y, if Y is going to dissapear in the
%     transformed code.
%
%   * X $@ Y
%     Print the term X following the format of Y.
%
%   * $@(X)
%     Use write_term for X (this will ignore automatic formatting following the
%     pattern)
%
%   * '$G'(Into, Goal)
%     Hook to execute Goal over the transformation generated by Into.
%
%   * '$NOOP'(X)
%     Just Ignore, but process X to get possible expected side effects (for
%     instance, '$G'/2 hacks).
%
%   * '$BODY'(X, Offset)
%     Print X as if it where the body of a clause, that is, by introducing a new
%     line per each conjunction and indentation starting at Offset position,
%     plus extra characters if required.
%
%   * '$BODY'(X)
%     Like '$BODY'(X, 0)
%
%   * '$BODYB'(X, Offset)
%     Like '$BODY', but adding braces if required
%
%   * '$BODYB'(X)
%     Like '$BODYB'(X, 0)
%
%   * '$CLAUSE'(X, Offset)
%     Print X as if it where a clause, starting indentation at Offset position.
%
%   * '$CLAUSE'(X)
%     Like '$CLAUSE'(X, 0)
%
%   * '$LIST'(L)
%     Print each element of L
%
%   * '$LISTC'(L)
%     Print each element of L in a way similar to portray_clause
%
%   * '$LIST,'(L)
%     Print each element of L placing a comma between them
%
%   * '$LIST,_'(L)
%     Print each element of L followed by a comma
%
%   * '$LIST,NL'(L)
%     Print each element of L followed by a comma and a new line If Level is
%     sent, the tool will add this automatically if the replacement is a list,
%     and in the case of an empty list, the sentence will be removed.
%
%   * '$LIST.NL'(L)
%     Print each element of L followed by a dot and a new line without clause
%     layout
%
%   * '$TEXT'(T, N)
%      Write T with higest priority and no quoted, byasing N characters to the
%      right
%
%   * '$TEXT'(T)
%     like '$TEXT'(T, 0)
%
%   * '$TEXTQ'(T, N)
%     Like '$TEXT'(T, N) but quoted
%
%   * '$TEXTQ'(T)
%     like '$TEXTQ'(T, 0)
%
%   * '$POS'(Name, Term)
%     Preserves the current write position in Name, for further usage in hacks
%     that have Offset as argument
%
%   * '$OUTPOS'
%     In an Offset expression, is replaced by the current write position.
%     For example:
%     ```
%     '$TEXT'(T,'$OUTPOS')
%     ```
%     is equivalent to:
%     ```
%     '$POS'(my_outpos, '$TEXT'(T, my_outpos))
%     ```
%
%   * '$SEEK'(T, O)
%     Seek 0 in the current output before to print T.
%
%   * '$TAB'(T, O)
%     Print as many spaces as needed to make O the current write position
%
%   Defined options are:
%
%   * fixpoint(+Value)
%     States that the replacement should be applied recursively, until no more
%     modifications are caused by the replacement.
%
%     Value=decreasing is the default, and means that the recursion stops if the
%     transformed term contains more terms that could potentially match to avoid
%     loops.  If the level is a non recursive one (see level_rec/2), such value
%     is equivalent to none.
%
%     Value=file means that the recursion is performed over the hole file.
%
%     Value=term means that the recursion is performed over the transformed term.
%
%     Value=true means that the recursion is applied up to reach the fixpoint
%     without decreasing control. If Level is a non recursive one, the recursion
%     is performed over the hole file, otherwise the recursion is only applied
%     over the transformed term.
%
%     Value=none don't apply the fixpoint algorithm.
%
%   * decrease_metric(:Metric) is a predicate of arity 3 of the form
%     predicate(+Term, +Pattern, -Size) to define the metric used to perform the
%     decreasing control (by default pattern_size/3).

replace(Level, Term, Into, Expander, MOptions) :-
    %% At this point we are not interested in styles
    meta_options(replace_meta_option, MOptions, Options),
    with_styles(with_counters(do_replace(Level, Term, Into, Expander, Options),
                              Options), [-singleton]).

replace_meta_option(decrease_metric).

%!  refactor_context(?Name, ?Value) is nondet.

refactor_context(Name, Value) :-
    get_context_value(Name, Value).

curr_style(Style, CurrStyle) :-
    arg(1, Style, Name),
    ( style_check(?(Name))
    ->CurrStyle = +Name
    ; CurrStyle = -Name
    ).

with_styles(Goal, StyleL) :-
    maplist(curr_style, StyleL, OldStyleL),
    setup_call_cleanup(maplist(style_check, StyleL),
                       Goal,
                       maplist(style_check, OldStyleL)).

% Note: To avoid that this hook be applied more than once, we record the
% positions already refactorized in ref_position/3.
%

remove_attribute(Attr, Var) :-
    del_attr(Var, Attr).

:- public do_goal_expansion/2.

do_goal_expansion(Term, TermPos) :-
    compound(TermPos),
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    nonvar(From),
    nonvar(To),
    refactor_context(file, File),
    \+ ref_position(File, From, To),
    assertz(ref_position(File, From, To)),
    term_variables(Term, Vars),
    ( refactor_context(cleanup_attributes, yes)
    ->maplist(remove_attribute('$var_info'), Vars)
    ; true
    ),
    refactor_context(goal_args, ga(Pattern, Into, Expander)),
    '$current_source_module'(M),
    b_getval('$variable_names', VNL),
    with_context_values(
        forall(substitute_term_norec(sub, M, Term, 999, Pattern, Into, Expander,
                                     TermPos, TermPos, Command),
               assertz(command_db(Command))),
        [variable_names],
        [VNL]).

do_replace(Level, Term, Into, Expander, Options) :-
    setup_call_cleanup(
        prepare_level(Level, Ref),
        apply_ec_term_level(Level, Term, Into, Expander, Options),
        cleanup_level(Level, Ref)).

prepare_level(goal, Ref) :-
    !,
    asserta((system:goal_expansion(G, P, _, _) :-
                 once(do_goal_expansion(G, P)),fail), Ref).
prepare_level(_, _).

cleanup_level(goal, Ref) :- !,
    erase(Ref),
    retractall(ref_position(_, _, _)).
cleanup_level(_, _).

apply_ec_term_level(Level, Term, Into, Expander, Options) :-
    forall(ec_term_level_each(Level, Term, Into, Expander, Options), true).

with_counters(Goal, Options1) :-
    foldl(select_option_default,
          [max_tries(MaxTries)-MaxTries],
          Options1, Options),
    with_context_values(
        ( Goal,
          refactor_context(count, Count),
          refactor_context(tries, Tries),
          foldl(select_option_default,
                [changes(Count)-Count,
                 tries(Tries)  -Tries],
                Options, _),
          message_type(Type),
          print_message(Type,
                        format("~w changes of ~w attempts", [Count, Tries]))
        ),
        [count,
         tries,
         max_tries],
        [0,
         0,
         MaxTries]
    ).

:- public clause_file_module/3.

param_file_module(clause(CRef), M, File) :-
    clause_property(CRef, file(File)),
    clause_property(CRef, module(M)).
param_file_module(mfiled(MFileD), M, File) :-
    get_dict(M1, MFileD, FileD),
    ( M1 = (-)
    ->true
    ; M = M1
    ),
    get_dict(File, FileD, _).

ec_term_level_each(Level, Term, Into, Expander, Options1) :-
    (Level = goal -> DExpand=yes ; DExpand = no),
    (Level = sent -> SentPattern = Term ; true), % speed up
    option(module(M), Options1, M),
    foldl(select_option_default,
          [syntax_errors(SE)-error,
           subterm_positions(TermPos)-TermPos,
           term_position(Pos)-Pos,
           linear_term(LinearTerm)-no,
           sentence(SentPattern)-SentPattern,
           comments(Comments)-Comments,
           expand(Expand)-DExpand,
           expanded(Expanded)-Expanded,
           cleanup_attributes(CleanupAttributes)-yes,
           fixpoint(FixPoint)-decreasing,
           max_changes(Max)-Max,
           variable_names(VNL)-VNL,
           vars_preffix(Preffix)-'V',
            % By default refactor even non loaded files
           if(Loaded)-true
          ],
          Options1, Options2),
    ( option(clause(CRef), Options2)
    ->MFileParam = clause(CRef),
      clause_property(CRef, line_count(Line)),
      merge_options([line(Line)], Options2, Options3)
    ; option_module_files([if(Loaded)|Options2], MFileD),
      MFileParam = mfiled(MFileD),
      Options3 = Options2
    ),
    Options = [syntax_errors(SE),
               subterm_positions(TermPos),
               variable_names(VNL),
               comments(Comments)|Options3],
    maplist(set_context_value,
            [sent_pattern,
             sentence,
             expanded,
             options,
             comments,
             bindings,
             subpos,
             pos,
             file,
             preffix,
             goal_args,
             cleanup_attributes,
             modified],
            [SentPattern,
             Linear,
             Expanded,
             Options,
             Comments,
             Bindings,
             TermPos,
             Pos,
             File,
             Preffix,
             ga(Term, Into, Expander),
             CleanupAttributes,
             false]),
    setup_call_cleanup(
        ( '$current_source_module'(OldM),
          freeze(M, '$set_source_module'(_, M))
        ),
        ( index_change(Index),
          param_file_module(MFileParam, M, File),
          fetch_sentence_file(
              Index, FixPoint, Max, M, File, SentPattern, Options, Expand,
              TermPos, VNL, Expanded, LinearTerm, Linear, Bindings, Level, Term,
              Into, Expander)
        ),
        '$set_source_module'(_, OldM)).

fixpoint_file(none, _, Goal) :- ignore(Goal).
fixpoint_file(true, Max, Goal) :-
    repeat,
      set_context_value(modified, false),
      ignore(Goal),
      refactor_context(count, Count),
      ( nonvar(Max),
        Count >= Max
      ->!
      ; true
      ),
      ( refactor_context(modified, false)
      ->!
      ; print_message(informational,
                      format("Restarting expansion", [])),
        fail
      ).

rec_fixpoint_file(rec,   P, F) :- rec_ff(P, F).
rec_fixpoint_file(norec, P, F) :- norec_ff(P, F).

rec_ff(decreasing, none).
rec_ff(file,       true).
rec_ff(term,       none).
rec_ff(true,       none).
rec_ff(none,       none).

norec_ff(decreasing, none).
norec_ff(file,       true).
norec_ff(term,       none).
norec_ff(true,       true).
norec_ff(none,       none).

fetch_sentence_file(Index, FixPoint, Max, M, File, SentPattern, Options,
                    Expand, TermPos, VNL, Expanded, LinearTerm,
                    Linear, Bindings, Level, Term, Into, Expander) :-
    level_rec(Level, Rec),
    rec_fixpoint_file(Rec, FixPoint, FPFile),
    fixpoint_file(
        FPFile, Max,
        apply_commands(
            Index, File, Level, M, Rec, FixPoint, Max,
            gen_module_command(
                SentPattern, Options, Expand, TermPos, Expanded, LinearTerm,
                Linear, VNL, Bindings, Term, Into, Expander))).

binding_varname(VNL, Var=Term) -->
    ( { member(Name=Var1, VNL),
        Var1==Term
      }
    ->[Name=Var]
    ; []
    ).

collect_singletons(Term, VNL, SVarL) :-
    term_variables(Term, VarU),
    sort(VarU, VarL),
    term_variables(VNL, UnnU),
    sort(UnnU, UnnL),
    ord_subtract(VarL, UnnL, SVarL).

gen_module_command(SentPattern, Options, Expand, TermPos, Expanded, LinearTerm,
                   Linear, VNL, Bindings, Term, Into, Expander, Level, M, Cmd, In) :-
    ref_fetch_term_info(SentPattern, Sent, Options, In, Once),
    b_setval('$variable_names', VNL),
    expand_if_required(Expand, M, Sent, TermPos, In, Expanded),
    make_linear_if_required(Sent, LinearTerm, Linear, Bindings),
    foldl(binding_varname(VNL), Bindings, RVNL, VNL),
    collect_singletons(Linear, RVNL, SVarL),
    S = solved(no),
    ( true
    ; arg(1, S, yes)
    ->cond_cut_once(Once),
      fail
    ),
    set_context_value(singletons, SVarL),
    set_context_value(variable_names, RVNL),
    substitute_term_level(Level, M, Linear, 1200, Term,
                          Into, Expander, TermPos, Cmd),
    nb_setarg(1, S, yes).

cond_cut_once(once).
cond_cut_once(mult(CP)) :- prolog_cut_to(CP).

ref_fetch_term_info(SentPattern, Sent, Options, In, once) :-
    nonvar(SentPattern),
    memberchk(SentPattern, [[], end_of_file]),
    !,
    option(comments([]), Options),
    ref_term_info_file(SentPattern, Sent, Options, In).
ref_fetch_term_info(SentPattern, Sent, Options, In, mult(CP)) :-
    repeat,
      prolog_current_choice(CP),
      ( fetch_term_info(SentPattern, Sent, Options, In)
      ; !,
        fail
      ).

ref_term_info_file(end_of_file, end_of_file, Options, In) :-
    seek(In, 0, eof, Size),
    option(subterm_positions(Size-Size), Options).
ref_term_info_file([], [], Options, _) :-
    option(subterm_positions(0-0), Options).

expand_if_required(Expand, M, Sent, TermPos, In, Expanded) :-
    ( Expand = no
    ->Expanded = Sent
    ; '$expand':expand_terms(prolog_source:expand, Sent, TermPos, In, Expanded)
    ),
    '$set_source_module'(CM, CM),
    M = CM,
    prolog_source:update_state(Sent, Expanded, M).

make_linear_if_required(Sent, LinearTerm, Linear, Bindings) :-
    ( LinearTerm = no
    ->Sent=Linear,
      Bindings = []
    ; mklinear(Sent, Linear, Bindings)
    ).

prolog:xref_open_source(File, Fd) :-
    nb_current(ti_open_source, yes),
    !,
    ( pending_change(_, File, Text)
    ->true
    ; read_file_to_string(File, Text, [])
    ),
    open_codes_stream(Text, Fd).
    % set_context_value(text, Text). % NOTE: update_state/2 have the side effect of
                                     % modify refactor_text

substitute_term_level(goal, _, _, _, _, _, _, _, Cmd) :-
    retract(command_db(Cmd)).
substitute_term_level(term, M, Sent, Priority, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_rec(M, Sent, Priority, Term, Into, Expander, TermPos, TermPos, Cmd).
substitute_term_level(sent, M, Sent, Priority, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_norec(top, M, Sent, Priority, Term, Into, Expander, TermPos, TermPos, Cmd).
substitute_term_level(head, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_head(norec, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd).
substitute_term_level(head_rec, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_head(rec, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd).
substitute_term_level(body, M, Clause, _, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_body(norec, M, Clause, Term, Into, Expander, TermPos, TermPos, Cmd).
substitute_term_level(body_rec, M, Clause, _, Term, Into, Expander, TermPos, Cmd) :-
    substitute_term_body(rec, M, Clause, Term, Into, Expander, TermPos, TermPos, Cmd).

substitute_term_body(Rec, M, Clause, Term, Into, Expander,
                     parentheses_term_position(_, _, TermPos), OutPos, Cmd) :- !,
    substitute_term_body(Rec, M, Clause, Term, Into, Expander, TermPos, OutPos, Cmd).
substitute_term_body(Rec, M, (_ :- Body), Term, Into, Expander,
                     term_position(_, _, _, _, [_, BodyPos]), OutPos, Cmd) :-
    term_priority((_ :- Body), M, 2, Priority),
    substitute_term(Rec, sub, M, Body, Priority, Term, Into, Expander, BodyPos, OutPos, Cmd).

substitute_term_head(Rec, M, Clause, Priority, Term, Into, Expander,
                     parentheses_term_position(_, _, TermPos), Cmd) :- !,
    substitute_term_head(Rec, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd).
substitute_term_head(Rec, M, Clause, Priority, Term, Into, Expander, TermPos, Cmd) :-
    ( Clause = (MHead :- _)
    ->( nonvar(MHead),
        MHead = IM:Head
      ->term_priority(IM:Head, M, 2, HPriority),
        term_position(_, _, _, _, [MHPos, _]) = TermPos,
        mhead_pos(MHPos, HeadPos)
      ; Head = MHead,
        term_priority(Clause, M, 1, HPriority),
        term_position(_, _, _, _, [HeadPos, _]) = TermPos
      )
    ; Clause \= (:- _),
      Head = Clause,
      HPriority = Priority,
      HeadPos = TermPos
    ),
    substitute_term(Rec, sub, M, Head, HPriority, Term, Into, Expander, HeadPos, TermPos, Cmd).

mhead_pos(parentheses_term_position(_, _, Pos), HPos) :- !, mhead_pos(Pos, HPos).
mhead_pos(term_position(_, _, _, _, [_, HPos]), HPos).

substitute_term(rec,   _,     M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd) :-
    substitute_term_rec(M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd).
substitute_term(norec, Level, M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd) :-
    substitute_term_norec(Level, M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd).

with_from(Goal, From) :-
    with_context_values(Goal, [from], [From]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANCILLARY PREDICATES:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

level_rec(goal,     norec).
level_rec(term,     rec).
level_rec(sent,     norec).
level_rec(head,     norec).
level_rec(head_rec, rec).
level_rec(body,     norec).
level_rec(body_rec, rec).

rec_fixpoint_term(norec, _, not).
rec_fixpoint_term(rec,   P, F) :- rec_ft(P, F).

rec_ft(decreasing, dec).
rec_ft(file,       not).
rec_ft(term,       rec).
rec_ft(true,       rec).
rec_ft(none,       not).

% This is weird due to the operators
apply_commands(Index, File, Level, M, Rec, FixPoint, Max, GenMCmd) :-
    ( pending_change(_, File, Text1)
    ->true
    ; exists_file(File)
    ->read_file_to_string(File, Text1, [])
    ; Text1 = ""
    ),
    rec_fixpoint_term(Rec, FixPoint, FPTerm),
    with_context_values(
        with_source_file(
            File, In,
            apply_commands_stream(
                FPTerm, GenMCmd, Level, M, nocs, Max, In, Text1, Text)),
        [text, file], [Text1, File]),
        ( Text1 \= Text
        ->nb_set_context_value(modified, true),
          save_change(Index, File-Text)
        ; true
        ).

decreasing_recursion(nocs, _).
decreasing_recursion(subst(_, _, _, _, _, _, S1),
                     subst(_, _, _, _, _, _, S2)) :-
    freeze(S2, S1 > S2).

do_recursion(dec(G), C, G, C).
do_recursion(rec(G), _, G, nocs).

rec_command_info(not, _, not).
rec_command_info(rec, G, rec(C)) :- copy_term(G, C).
rec_command_info(dec, G, dec(C)) :- copy_term(G, C).

increase_counter(Count1) :-
    refactor_context(count, Count),
    succ(Count, Count1),
    nb_set_context_value(count, Count1).

do_genmcmd(GenMCmd, Level, M, CS, Command, In, Max) :-
    decreasing_recursion(CS, Command),
    call(GenMCmd, Level, M, Command, In),
    increase_counter(Count1),
    ( nonvar(Max),
      Count1 >= Max
    ->!
    ; true
    ).

apply_commands_stream(FPTerm, GenMCmd, Level, M, CS, Max, In, Text1, Text) :-
    IPosText = 0-"",
    rec_command_info(FPTerm, GenMCmd, CI),
    ignore(forall(do_genmcmd(GenMCmd, Level, M, CS, Command, In, Max),
                  apply_commands_stream_each(FPTerm, CI, M, Max, Command,
                                             Text1, IPosText)
                 )),
    IPosText = Pos-Text6,
    sub_string(Text1, Pos, _, 0, TText),
    string_concat(Text6, TText, Text).

apply_commands_stream_each(FPTerm, CI, M, Max, Command, Text1, IPosText) :-
    ( apply_change(Text1, M, Command, FromToPText1),
      ( do_recursion(CI, Command, GenMCmd, CS)
      ->FromToPText1 = t(From, To, PasteText),
        get_out_pos(Text1, IPosText, From, Line, LPos),
        with_output_to(atom(LeftText),
                       ( forall(between(2, Line, _), nl),
                         line_pos(LPos)
                       )),
        atomics_to_string([LeftText, PasteText, "."], Text2),
        set_context_value(text, Text2),
        setup_call_cleanup(
            ( open_codes_stream(Text2, In)
              % seek(In, Pos, bof, _)
            ),
            apply_commands_stream(FPTerm, GenMCmd, term, M, CS, Max, In,
                                  Text2, Text3),
            close(In)),
        set_context_value(text, Text1),
        string_concat(Text4, ".", Text3),
        string_concat(LeftText, Text5, Text4),
        FromToPText = t(From, To, Text5)
      ; FromToPText = FromToPText1
      ),
      string_concat_to(Text1, FromToPText, IPosText, Pos-Text6),
      nb_setarg(1, IPosText, Pos),
      nb_setarg(2, IPosText, Text6)
    ).

get_out_pos(RText, Pos-Text1, From, Line, LPos) :-
    Length is max(0, From - Pos),
    sub_string(RText, Pos, Length, _, Text2),
    string_concat(Text1, Text2, Text3),
    textpos_line(Text3, From, Line, LPos).

string_concat_to(RText, t(From, To, PasteText), Pos-Text1, To-Text) :-
    Length is max(0, From - Pos),
    sub_string(RText, Pos, Length, _, Text2),
    string_concat(Text1, Text2, Text3),
    string_concat(Text3, PasteText, Text).

gen_new_variable_name(VNL, Preffix, Count, Name) :-
    atom_concat(Preffix, Count, Name),
    \+ member(Name=_, VNL), !.
gen_new_variable_name(VNL, Preffix, Count1, Name) :-
    succ(Count1, Count),
    gen_new_variable_name(VNL, Preffix, Count, Name).

will_occurs(Var, Sent, Pattern, Into, VNL, T) :-
    findall(N,
            ( member(Name=Var1, VNL),
              Name \= '_',
              Var==Var1
            ->member(Name=Var2, VNL),
              will_occurs(Var2, Sent, Pattern, Into, N)
            ; will_occurs(Var,  Sent, Pattern, Into, N)
            ), NL),
    sumlist(NL, T).

will_occurs(Var, Sent, Pattern, Into, N) :-
    occurrences_of_var(Var, Sent, SN),
    occurrences_of_var(Var, Pattern, PN),
    occurrences_of_var(Var, Into, IN),
    N is SN-PN+IN.

gen_new_variable_names([], _, _, _, _, _, _, _, VNL, VNL).
gen_new_variable_names([Var|VarL], [Name1|NameL], SVarL, Preffix, Count1,
                       Sent, Pattern, Into, VNL1, VNL) :-
    ( nonvar(Name1)
    ->VNL2 = VNL1,
      Count = Count1
    ; will_occurs(Var, Sent, Pattern, Into, VNL1, N),
      N > 1
    ->gen_new_variable_name(VNL1, Preffix, Count1, Name),
      succ(Count1, Count),
      VNL2 = [Name=Var|VNL1]
    ; ( member(Var1, SVarL),
        Var1 == Var
      ->VNL2 = VNL1
      ; VNL2 = ['_'=Var|VNL1]
      ),
      Count = Count1
    ),
    gen_new_variable_names(VarL, NameL, SVarL, Preffix, Count, Sent, Pattern, Into, VNL2, VNL).

with_termpos(Goal, TermPos) :-
    with_context_values(Goal, [termpos], [TermPos]).

apply_change(Text, M, subst(TermPos, Priority, Pattern, Term, VNL, Into, _),
             t(From, To, PasteText)) :-
    wr_options(Options),
    call_cleanup(
        with_output_to(
            string(OutputText),
            ( stream_property(current_output, position(Pos1)),
              with_termpos(
                  print_expansion_0(Into, Pattern, Term, TermPos,
                                    [priority(Priority), module(M),
                                     variable_names(VNL)
                                     |Options],
                                    Text, From, To),
                  TermPos),
              stream_property(current_output, position(Pos2))
            )),
        retractall(rportray_pos(_, _))),
    stream_position_data(char_count, Pos1, B1),
    stream_position_data(char_count, Pos2, B2),
    get_subtext(OutputText, B1, B2, PasteText).

wr_options([portray_goal(ref_replace:rportray),
            spacing(next_argument),
            numbervars(true),
            quoted(true),
            partial(true),
            character_escapes(false)]).

print_expansion_0(Into, Pattern, Term, TermPos, Options, Text, From, To) :-
    arg(1, TermPos, OFrom),
    arg(2, TermPos, OTo),
    get_innerpos(OFrom, OTo, IFrom, ITo),
    nb_setarg(1, TermPos, IFrom),
    nb_setarg(2, TermPos, ITo),
    ( nonvar(Into)
    ->print_expansion_1(Into, Pattern, Term, TermPos, Options, Text, From, To)
    ; print_expansion_2(Into, Pattern, Term, TermPos, Options, Text, From, To)
    ).

with_pattern_into(Goal, Pattern, Into) :-
    refactor_context(tries, Tries),
    refactor_context(max_tries, MaxTries),
    ( nonvar(MaxTries)
    ->Tries < MaxTries
    ; true
    ),
    succ(Tries, Tries1),
    nb_set_context_value(tries, Tries1),
    with_context_values(catch(once(Goal), Error,
                              ( refactor_message(error, Error),
                                fail
                              )),
                        [pattern, into],
                        [Pattern, Into]).

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

with_context(Sent, Term, Pattern1, Into1, Pattern, Into, VNL, Goal) :-
    copy_term(Pattern1-Into1, Pattern-Into2),
    refactor_context(sentence, Sent),
    refactor_context(sent_pattern, Sent),
    Pattern1 = Term,
    copy_term(Term-Into1, Term2-Into3),
    with_pattern_into(Goal, Pattern, Into2), % Allow changes in Pattern1/Into1
    term_variables(Pattern, Vars2), % Variable bindings in Pattern
    %% Apply changes to Pattern/Into and bind Vars:
    copy_term(t(Pattern, Into2, Vars2), t(Pattern1, Into1, Vars1)),
    copy_term(t(Pattern, Into2, Vars2), t(Term2,    Into3, Vars3)),
    gen_new_variable_names(Sent, Term, Into1, VNL),
    pairs_keys_values(Pairs, Vars1, Vars2),
    pairs_keys_values(Triplets, Pairs, Vars3),
    map_subterms(p(Triplets, Pattern1, Pattern, Term2, VNL, Into2, Into3), Into1, Into2, Into).

map_subterms(Params, T1, T2, T) :-
    Params = p(Triplets, P1, P2, P3, VNL, Into1, Into2),
    ( member(X1-X2-X3, Triplets),
      X2 == T2
    ; member(X1-X2-X3, Triplets),
      same_term(X1, T1)         % ===/2
    ; sub_term(P1, P2, P3, X1, X2, X3),
      \+ atomic(X1), % Special case: ignore atomics
      same_term(X1, T1)         % ===/2
    ; sub_term(P1, P2, P3, X1, X2, X3),
      \+ atomic(X1), % Special case: ignore atomics
      X2 == T2
    ),
    !,
    ( T1 == T2
    ->T = X1
    ; \+ ( member(_=V1, VNL),
           sub_term(V2, X1),
           var(V2),
           V2 == V1
         )
    ->( X2 =@= X3,
        \+ same_term(X1, T1),
        occurrences_of_var(X2, Into1, N),
        occurrences_of_var(X3, Into2, N)
      ->T = X1
      ; T = X2
      )
    ; map_subterms_2(Params, T1, T2, T)
    ).
map_subterms(Params, T1, T2, T) :-
    map_subterms_2(Params, T1, T2, T).

sub_term(P1, P2, P3, P1, P2, P3).
sub_term(P1, P2, P3, X1, X2, X3) :-
    compound(P1),
    functor(P1, F, N),
    functor(P2, F, N),
    functor(P3, F, N),
    arg(I, P1, A1),
    arg(I, P2, A2),
    arg(I, P3, A3),
    sub_term(A1, A2, A3, X1, X2, X3).

map_subterms_2(Params, T1, T2, T) :-
    compound(T1), !,
    map_compound(Params, T1, T2, T).
map_subterms_2(_, T, _, T).

map_compound(Params,            % Special case: preserve Goal
             '$G'(T1, G),
             '$G'(T2, _),
             '$G'(T,  G)) :- !,
    map_subterms(Params, T1, T2, T).
map_compound(Params,            % Special case: preserve Goal
             '$C'(G, T1),
             '$C'(_, T2),
             '$C'(G, T )) :- !,
    map_subterms(Params, T1, T2, T).
map_compound(Params,
             '$@'(X1, Y1),
             '$@'(X2, Y2),
             '$@'(X,  Y)) :- !,
    map_subterms(Params, X1, X2, X),
    Params=p(Triplets, P1, P2, P3, _, Into1, Into2),
    map_subterms(p(Triplets, P1, P2, P3, [], Into1, Into2), Y1, Y2, Y).
map_compound(Params,
             '@@'(X1, Y1),
             '@@'(X2, Y2),
             '@@'(X,  Y)) :- !,
    map_subterms(Params, X1, X2, X),
    Params=p(Triplets, P1, P2, P3, _, Into1, Into2),
    map_subterms(p(Triplets, P1, P2, P3, [], Into1, Into2), Y1, Y2, Y).
map_compound(Params, T1, T2, T) :-
    functor(T1, F, N),
    functor(T2, F, N),
    functor(T,  F, N),
    T1 =.. [F|Args1],
    T2 =.. [F|Args2],
    T  =.. [F|Args],
    maplist(map_subterms(Params), Args1, Args2, Args).

special_term(top,    Pattern, Term, '$LISTC'(List)) :-
    nonvar(Pattern),
    memberchk(Pattern, [[], end_of_file]), !,
    ( \+ is_list(Term)
    ->List = [Term]
    ; List = Term
    ).
special_term(sub_cw, _, Term,  Term).
special_term(sub,    _, Term,  Term).
special_term(top,    _, Term1, Term) :- top_term(Term1, Term).

top_term(Var, Var) :- var(Var), !.
top_term(List, '$LISTC.NL'(List)) :- List = [_|_], !.
top_term([], '$RM') :- !.
top_term(Term, Term).

trim_hacks(Term, Trim) :-
    substitute(trim_hack, Term, Trim).

trim_hack(Term, Trim) :-
    nonvar(Term),
    do_trim_hack(Term, Trim1),
    trim_hacks(Trim1, Trim).

do_trim_hack('$@'(Term, _), Term).
do_trim_hack('@@'(Term, _), Term).
do_trim_hack(\\(Term), Term).
do_trim_hack('$NOOP'(_), '').

match_vars_with_names(VNL1, Var, Name) :-
    ignore(( member(Name=Var1, VNL1),
             Var == Var1
           )).

gen_new_variable_names(Sent, Term, Into, VNL) :-
    refactor_context(preffix, Preffix),
    refactor_context(variable_names, VNL1),
    refactor_context(singletons, SVarL),
    trim_hacks(Into, TInto),
    term_variables(TInto, VarL),
    maplist(match_vars_with_names(VNL1), VarL, NameL),
    gen_new_variable_names(VarL, NameL, SVarL, Preffix, 1, Sent, Term, TInto, VNL1, VNL2),
    once(append(VNL, VNL1, VNL2)).

%!  substitute_term_norec(+Sub, +M, +Term, +Priority, +Pattern, +Into, :Expander, +TermPos, OutPos, Cmd) is nondet.
%
%   Non-recursive version of substitute_term_rec//6.

substitute_term_norec(Sub, M, Term, Priority, Pattern, Into1, Expander, TermPos1, OutPos,
                      subst(TermPos, Priority, Pattern3, GTerm, VNL, Into, Size)) :-
    refactor_context(sentence,     Sent),
    refactor_context(sent_pattern, SentPattern),
    subsumes_term(SentPattern-Pattern, Sent-Term),
    copy_term(Sent, Sent2),
    refactor_context(options, Options),
    option(decrease_metric(Metric), Options, ref_replace:pattern_size),
    call(Metric, Term, Pattern, Size),
    with_termpos(( with_context(Sent, Term, Pattern, Into1, Pattern1, Into2, VNL, Expander),
                   check_bindings(Sent, Sent2, Options)
                 ), TermPos1),
    greatest_common_binding(Pattern1, Into2, Pattern2, Into3, [[]], Unifier, []),
    perform_substitution(Sub, Priority, M, Term, VNL, Pattern2, Into3, Unifier,
                         TermPos1, OutPos, Options, TermPos, Pattern3, GTerm, Into).

check_bindings(Sent, Sent2, Options) :-
    ( Sent=@=Sent2
    ->true
    ; option(show_left_bindings(Show), Options, false),
      ( Show = true
      ->refactor_message(warning, format("Bindings occurs: ~w \\=@= ~w.", [Sent2, Sent]))
      ; true
      )
    ).

:- public
       pattern_size/3.

pattern_size(Term, Pattern, Size) :-
    findall(S,
            ( sub_term(Sub, Term),
              subsumes_term(Pattern, Sub),
              term_size(Sub, S)
            ), SL),
    sum_list(SL, Size).

fix_subtermpos(Pattern, _, _, _, _) :-
    nonvar(Pattern),
    memberchk(Pattern, [[], end_of_file]), !.
fix_subtermpos(_, Into, Sub, TermPos, Options) :-
    fix_subtermpos(Sub, Into, TermPos, Options).

fix_subtermpos(sub_cw, _,    _, _). % Do nothing
fix_subtermpos(sub,    _,    TermPos, Options) :- fix_subtermpos(TermPos, Options).
fix_subtermpos(top,    Into, TermPos, Options) :-
    ( Into \= [_|_]
    ->fix_termpos(   TermPos, Options)
    ; fix_subtermpos(TermPos, Options)
    ).

%!  perform_substitution(+Sub, +Priority, +M, +Term, +VNL, +Pattern1, +Into1, +BindingL, +TermPos1, +OutPos, +Options, -TermPos, -Pattern, -GTerm, -Into)
%
%   Substitute occurences of Pattern with Into after calling
%   expansion.
%
%   @param Term is the term as read from the source
%   @param TermPos is the term layout of SrcTerm
%   @param OutPos layout of the term that includes SrcTerm
%   @param Priority is the environment operator priority
%
perform_substitution(Sub, Priority, M, Term, VNL, Pattern1, Into1, BindingL,
                     TermPos1, OutPos1, Options, TermPos, Pattern, GTerm, Into) :-
    ( trim_fake_pos(TermPos1, TermPos, N)
    ->substitute_value(TermPos1, TermPos, OutPos1, OutPos),
      trim_fake_args(N, Pattern1, Pattern),
      trim_fake_args(N, Into1, Into2),
      trim_fake_args(N, Term,  Term1)
    ; Pattern = Pattern1,
      Into2 = Into1,
      Term1 = Term,
      TermPos = TermPos1,
      OutPos = OutPos1
    ),
    copy_term(t(Term1), t(GTerm)),
    /* Note: fix_subtermpos/1 is a very expensive predicate, due to that we
       delay its execution until its result be really needed, and we only
       apply it to the subterm positions being affected by the refactoring.
       The predicate performs destructive assignment (as in imperative
       languages), modifying term position once the predicate is called */
    fix_subtermpos(Pattern, Into2, Sub, OutPos, Options),
    with_context_values(subst_term(TermPos, M, Pattern, GTerm, Priority, Term1),
                        [subst_vars, bind, new_varnames], [[], BindingL, VNL]),
    shared_variables(VNL, Term1, Into2, V5), % after subst_term, in case some
    maplist(\ A^B^(A=B)^true, V5, V5, UL5),  % variables from Term3 reappear
    maplist(subst_fvar(M, Term1, TermPos, GTerm), UL5),
    special_term(Sub, Pattern, Into2, Into),
    maplist(collapse_bindings, BindingL), % This looks like a kludge (test bind1)
    !.

collapse_bindings(A=B) :- ignore(A=B).

subst_fvar(M, Term, Pos, GTerm, V=T) :-
    ( var(V),
      V==T,
      get_position_gterm(M, Term, Pos, GTerm, V, GPos, _G, _P)
    ->V='$sb'(GPos)
    ; true % already unified
    ).

%!  trim_fake_pos(+TermPos, -Pos, -N)
%
%   remove fake arguments that would be added by dcg
trim_fake_pos(term_position(F, T, FF, FT, PosL1), Pos, N) :-
    nonvar(PosL1),
    once(( member(FE, [0-0, T-T]),
           append(PosL, [FE|E], PosL1),
           maplist('='(FE), E)
         )),
    length([_|E], N),
    Pos = term_position(F, T, FF, FT, PosL).

trim_fake_args(N, Term1, Term) :-
    ( Term1 =.. ATerm1,
      length(TE, N),
      append(ATerm, TE, ATerm1),
      Term =.. ATerm
    ->true
    ; Term = Term1
    ).

shared_variables(VNL, Term1, Term2, Var) :-
    term_variables(Term1, Var1),
    term_variables(Term2, Var2),
    partition(is_eq(VNL, Var1), Var2, Var, _).

is_eq(VNL, Var1L, Var2) :-
    member(Var1, Var1L),
    Var1==Var2,
    \+ ( member(_=Var3, VNL),
         Var2==Var3
       ),
    !.

get_position_gterm(M, Term, Pos, GTerm, T, GPos, G, GPriority) :-
    subterm_location_eq(L, T, Term),
    subpos_location(L, Pos, GPos),
    ( append(L1, [E], L) ->
      subterm_location(L1, GP, GTerm),
      subterm_location([E], G, GP),
      term_priority(GP, M, E, GPriority)
    ; GPriority = 999,
      subterm_location(L, G, GTerm)
    ).

get_innerpos(From, To, IFrom, ITo) :-
    term_innerpos(From, To, IFrom, ITo),
    !.
get_innerpos(From, To, From, To).

subst_args(N, M, Term, GTerm, CTerm, [ArgPos|SubPos]) :-
    arg(N, Term,  Arg),
    !,
    arg(N, GTerm, GArg),
    arg(N, CTerm, CArg),
    term_priority(GTerm, M, N, GPriority),
    subst_term(ArgPos, M, Arg, GArg, GPriority, CArg),
    succ(N, N1),
    subst_args(N1, M, Term, GTerm, CTerm, SubPos).
subst_args(_, _, _, _, _, _).

subst_list([], M, _, Tail, E, G, C) :-
    term_priority([_|_], M, 2, P),
    subst_term(Tail, M, E, G, P, C).
subst_list([Pos|Poss], M, To, Tail, [E|Es], [G|Gs], [C|Cs]) :-
    term_priority([_|_], M, 1, P),
    subst_term(Pos, M, E, G, P, C),
    subst_list(Poss, M, To, Tail, Es, Gs, Cs).

subst_var(Pos, M, Var, GTerm, GPriority, CTerm) :-
    ( refactor_context(new_varnames, VNL),
      member(_=V, VNL),
      V==CTerm
    ->Var = CTerm
    ; ( refactor_context(bind, BindingL),
        member(V=T, BindingL),
        V==Var
      ->subst_term(Pos, M, T, GTerm, GPriority, CTerm)
      ; true
      ),
      arg(1, Pos, From),
      arg(2, Pos, To),
      get_innerpos(From, To, IFrom, ITo),
      Var = '$sb'(Pos, IFrom, ITo, GTerm, GPriority, CTerm),
      get_context_value(subst_vars, SVL),
      set_context_value(subst_vars, [CTerm|SVL])
    ).

%!  subst_term(+Position, +Module, +Pattern, +Term, +Priority, Subst)
%
%   Here, Pattern is a term  that   holds  variables.  It is matched
%   against a position term and  if  there   is  a  variable  in the
%   pattern, this is replaced by '$sb'(Pos, ...),
%   indicating that this position currently holds SubTerm.
%
%   @param Position is a subterm-position term for Term
%   @param Term is a source term
%   @param Pattern is a substitution pattern
%   @param Priority is the priority of the source Term
%   @param Subst is the resulting term after the replacement

subst_term(none, _, T, _, _, T) :- !.
subst_term(Pos, M, Term, GTerm, GPriority, CTerm) :-
    var(Term),
    !,
    subst_var(Pos, M, Term, GTerm, GPriority, CTerm).
subst_term(_, _, '$sb'(_, _, _, _, _, CTerm), _, _, CTerm) :-
    get_context_value(subst_vars, SVL),
    once(( member(SV, SVL),
           SV == CTerm
         )),
    !.
subst_term(term_position(_, _, _, _, CP), M, Term, GTerm, _, CTerm) :-
    compound(CTerm), % Would have been substituted
    !,
    subst_args(1, M, Term, GTerm, CTerm, CP).
subst_term(brace_term_position(_, _, CP), M, {Term}, {GTerm}, _, {CTerm}) :- !,
    subst_term(CP, M, Term, GTerm, 999, CTerm).
subst_term(parentheses_term_position(_, _, Pos), M, Term, GTerm, GP, CTerm) :- !,
    subst_term(Pos, M, Term, GTerm, GP, CTerm).
subst_term(list_position(_, To, Elms, Tail), M, Term, GTerm, _, CTerm) :- !,
    subst_list(Elms, M, To, Tail, Term, GTerm, CTerm).
subst_term(_, _, _, _, _, _).

%!  substitute_term_rec(+Module, +Term, +Priority, +Pattern, +Into, :Expander, +TermPos, +OutPos, Cmd) is nondet.
%
%   True when the DCG list contains a substitution for Pattern by Into in
%   SrcTerm. This predicate must be cautious about handling bindings:
%
%   - Overall bindings do not affect further substitutions because we are
%     managed by findall/3 in do_replace/6.
%   - Pattern must not be instantiated by either unification with SrcTerm or the
%     execution of Expander.  This is needed for substitute_term/7 to find the
%     correct replacements.
%
%   To avoid binding Pattern, we need to copy Pattern and Into while maintaining
%   sharing with Expander.  Next, we can safely unify Pattern with the SrcTerm.

substitute_term_rec(M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd) :-
    substitute_term_norec(sub, M, Term, Priority, Pattern, Into, Expander, TermPos, OutPos, Cmd),
    !.
substitute_term_rec(M, Term, _, Ref, Into, Expander, TermPos, _, Cmd) :-
    substitute_term_into(TermPos, TermPos, M, Term, Ref, Into, Expander, Cmd).

substitute_term_into(brace_term_position(_, _, Pos), OutPos, M, {Term}, Ref, In, Ex, Cmd) :-
    substitute_term_rec(M, Term, 1200, Ref, In, Ex, Pos, OutPos, Cmd).
substitute_term_into(parentheses_term_position(_, _, Pos), OutPos, M, Term, Ref, In, Ex, Cmd) :-
    substitute_term_rec(M, Term, 1200, Ref, In, Ex, Pos, OutPos, Cmd).
substitute_term_into(term_position(_, _, _, _, PosL), OutPos, M, Term, Ref, In, Ex, Cmd) :-
    substitute_term_args(PosL, OutPos, M, Term, Ref, In, Ex, Cmd).
substitute_term_into(list_position(_, _, EP, TP), OutPos, M, Term, Ref, In, Ex, Cmd) :-
    substitute_term_list(EP, TP, OutPos, M, Term, Ref, In, Ex, Cmd).
substitute_term_into(map_position(_, _, _, _, PosL), _, M, Term, Ref, In, Ex, Cmd) :-
    member(Pos, PosL),
    substitute_term_pair(M, Term, Ref, In, Ex, Pos, Pos, Cmd).

substitute_term_pair(M, Term, Ref, Into, Expander,
                     key_value_position(_, _, Key, PosK, PosV), OutPos, Cmd) :-
    ( substitute_term_rec(M, Key, 999, Ref, Into, Expander, PosK, OutPos, Cmd)
    ; substitute_term_rec(M, Term.Key, 999, Ref, Into, Expander, PosV, OutPos, Cmd)
    ).

:- use_module(library(listing), []).

term_priority(Term, M, N, Priority) :-
    nonvar(Term),
    term_priority_gnd(Term, M, N, PrG),
    ( arg(N, Term, Arg),
      term_needs_braces(M:Arg, PrG)
    ->Priority=999
    ; Priority=PrG
    ).

term_priority_gnd(Term, M, N, PrG) :-
    functor(Term, F, A),
    ( ( A == 1 ->
        ( prolog_listing:prefix_op(M:F, PrG) -> true
        ; prolog_listing:postfix_op(M:F, PrG) -> true
        )
      ; A == 2 ->
        prolog_listing:infix_op(M:F, Left, Right),
        ( N==1 -> PrG = Left
        ; N==2 -> PrG = Right
        )
      )
    ->true
    ; PrG=999           % term_priority((_, _), 1, Priority)
    ).

substitute_term_args(PAL, OutPos, M, Term, Ref, Into, Expander, Cmd) :-
    nth1(N, PAL, PA),
    arg(N, Term, Arg),
    term_priority(Term, M, N, Priority),
    substitute_term_rec(M, Arg, Priority, Ref, Into, Expander, PA, OutPos, Cmd).

substitute_term_list([EP|EPs], TP, OutPos, M, [Elem|Term], Ref, Into, Expander, Cmd) :-
    ( term_priority([_|_], M, 1, Priority),
      substitute_term_rec(M, Elem, Priority, Ref, Into, Expander, EP, OutPos, Cmd)
    ; substitute_term_list(EPs, TP, OutPos, M, Term, Ref, Into, Expander, Cmd)
    ).
substitute_term_list([], TP, OutPos, M, Tail, Ref, Into, Expander, Cmd) :-
    term_priority([_|_], M, 2, Priority),
    substitute_term_rec(M, Tail, Priority, Ref, Into, Expander, TP, OutPos, Cmd).

compound_positions(Line1, Pos2, Pos1, Pos) :-
    Line1 =< 1,
    !,
    Pos is Pos1 + Pos2.
compound_positions(_, Pos, _, Pos).

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

get_output_position(Pos) :-
    ( current_context_value(from, From)
    ->true
    ; From = 0
    ),
    get_output_position(From, Pos).

get_output_position(From, Pos) :-
    refactor_context(text, Text),
    textpos_line(Text, From, _Line1, Pos1),
    stream_property(current_output, position(StrPos)),
    stream_position_data(line_count, StrPos, Line1),
    stream_position_data(line_position, StrPos, Pos2),
    compound_positions(Line1, Pos2, Pos1, Pos).

write_term_dot_nl(Term, OptL) :-
    write_term(Term, OptL),
    write('.\n').
rportray_clause_dot_nl(Clause, OptL) :-
    rportray_clause(Clause, OptL),
    write('.\n').

rportray_clause(Clause, OptL) :-
    rportray_clause(Clause, 0, OptL).

% We can not use portray_clause/3 because it does not handle the hooks
% portray_clause_(OptL, Clause) :-
%     portray_clause(current_output, Clause, OptL).

rportray_clause(C, Pos, OptL1) :-
    option(module(M), OptL1),
    stream_property(current_output, position(SPos1)),
    merge_options([portray_clause(false)], OptL1, OptL2),
    write_term(C, OptL2),
    stream_property(current_output, position(SPos2)),
    ( nonvar(C),
      ( stream_position_data(line_count, SPos1, Line1),
        stream_position_data(line_count, SPos2, Line2),
        Line1 \= Line2
      ; stream_position_data(line_position, SPos2, Pos2),
        Pos2 > 80
      )
    ->set_stream_position(current_output, SPos1),
      ( option(priority(CPri), OptL1),
        term_needs_braces(C, M, CPri)
      ->Display = yes,
        succ(Pos, BPos)
      ; Display = no,
        BPos = Pos
      ),
      cond_display(Display, '('),
      merge_options([portray_clause(true)], OptL1, OptL3),
      ( memberchk(C, [(H :- B), (H --> B)])
      ->write_term(H, OptL3),
        functor(C, Neck, _),
        write(' '),
        writeln(Neck),
        line_pos(4 + BPos),
        term_priority((_, _), M, 2, Priority),
        merge_options([priority(Priority)], OptL3, OptL4),
        write_b(B, OptL4, 4 + BPos)
      ; write_term(C, OptL3)
      ),
      cond_display(Display, ')')
    ; true
    ).

deref_substitution('$sb'(_, _, _, _, _, Term), Term) :- !.
deref_substitution(Term, Term).

:- public rportray/2.
rportray('$sb'(TermPos), _) :-
    \+ retract(rportray_skip),
    !,
    refactor_context(text, Text),
    print_subtext(TermPos, Text).
rportray('$sb'(ArgPos, _IFrom, _ITo, GTerm, GPriority, Into), Options) :-
    \+ retract(rportray_skip),
    !,
    ignore((refactor_context(text, Text),
            print_subtext_sb(Into, GTerm, ArgPos, GPriority, Options, Text)
           )).
rportray('$@'(Term), Options) :-
    write_term(Term, Options).
rportray(\\(Term), Options) :-
    \+ retract(rportray_skip),
    !,
    assertz(rportray_skip),
    write_term(Term, Options).
% rportray('$sb'(_, _, _, _), _) :- !.
rportray(@@(Term, STerm), Options) :-
    \+ retract(rportray_skip),
    !,
    ( nonvar(STerm),
      STerm = '$sb'(TermPos, IFrom, ITo, _, _, _)
    ->arg(1, TermPos, From),
      arg(2, TermPos, To),
      refactor_context(text, Text),
      print_subtext(From-IFrom, Text),
      write_term(Term, Options),
      print_subtext(ITo-To, Text)
    ; write_term(Term, Options)
    ).
rportray('$@'(Into, '$sb'(ArgPos, _IFrom, _ITo, GTerm, GPriority, Pattern)), Options) :-
    !,
    % Use a different pattern to guide the printing of Term
    refactor_context(text, Text),
    print_expansion_sb(Into, Pattern, GTerm, ArgPos, GPriority, Options, Text),
    !.
rportray('$G'(Into, Goal), Opt) :-
    !,
    with_str_hook(write_term(Into, Opt), Goal).
rportray('$C'(Goal, Into), Opt) :-
    !,
    call(Goal),
    write_term(Into, Opt).
rportray('$NOOP'(Term), Opt) :- !,
    with_output_to(string(_),   % Ignore, but process for the side effects
                   write_term(Term, Opt)).
rportray('$TEXT'(T), Opt) :- !,
    write_t(T, Opt).
rportray('$TEXT'(T, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    line_pos(Pos),
    write_t(T, Opt).
rportray('$TEXTQ'(T), Opt) :- !,
    write_q(T, Opt).
rportray('$TEXTQ'(T, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    line_pos(Pos),
    write_q(T, Opt).
rportray(H :- B, Opt) :-
    option(portray_clause(true), Opt),
    !,
    offset_pos('$OUTPOS', Pos),
    rportray_clause((H :- B), Pos, Opt).
rportray('$CLAUSE'(C), Opt) :- !,
    rportray_clause(C, Opt).
rportray('$CLAUSE'(C, Offs), Opt) :- !,
    offset_pos(Offs, Pos),
    rportray_clause(C, Pos, Opt).
rportray('$BODY'(B, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    rportray_body(B, Pos, Opt).
rportray('$BODY'(B), Opt) :- !,
    offset_pos('$OUTPOS', Pos),
    rportray_body(B, Pos, Opt).
rportray('$BODYB'(B, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    rportray_bodyb(B, Pos, Opt).
rportray('$BODYB'(B), Opt) :- !,
    offset_pos('$OUTPOS', Pos),
    rportray_bodyb(B, Pos, Opt).
rportray('$POS'(Name, Term), Opt) :-
    get_output_position(Pos),
    nonvar(Name),
    ( \+ rportray_pos(Name, _)
    ->assertz(rportray_pos(Name, Pos))
    ; refactor_message(warning, format("Position named ~w redefined", [Name])),
      retractall(rportray_pos(Name, _)),
      assertz(rportray_pos(Name, Pos))
    ),
    write_term(Term, Opt).
rportray('$LIST'(L), Opt) :-
    !,
    rportray_list(L, write_term, '', Opt).
rportray('$LIST,'(L), Opt) :-
    !,
    rportray_list(L, write_term, ',', Opt).
rportray('$LIST,_'(L), Opt) :- !,
    maplist(term_write_comma_2(Opt), L).
rportray('$LIST'(L, Sep), Opt) :- !,
    rportray_list(L, write_term, Sep, Opt).
rportray('$LISTC'(CL), Opt) :- !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(CL, rportray_clause_dot_nl, '', Opt1).
rportray('$LISTC.NL'(CL), Opt) :- !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(CL, rportray_clause, '.\n', Opt1).
rportray('$LIST.NL'(L), Opt) :- !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(L, write_term_dot_nl, '', Opt1).
rportray('$LISTNL.'(L), Opt) :- !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(L, write_term, '.\n', Opt1).
rportray('$LIST,NL'(L), Opt) :-
    offset_pos('$OUTPOS', Pos), !,
    rportray_list_nl_comma(L, Pos, Opt).
rportray('$TAB'(Term, Offs), Opt) :-
    offset_pos(Offs-'$OUTPOS', Delta), !,
    forall(between(1, Delta, _), write(' ')),
    write_term(Term, Opt).
rportray('$LIST,NL'(L, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    rportray_list_nl_comma(L, Pos, Opt).
rportray('$LISTB,NL'(L), Opt) :- !,
    offset_pos('$OUTPOS'+1, Pos), !,
    deref_substitution(L, D),
    rportray_list_nl_b(D, Pos, Opt).
rportray('$LISTB,NL'(L, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    deref_substitution(L, D),
    rportray_list_nl_b(D, Pos, Opt).
rportray('$NL'(Term, Offs), Opt) :-
    offset_pos(Offs, Pos), !,
    nl,
    line_pos(Pos),
    write(''),
    write_term(Term, Opt).
rportray('$SEEK'(Term, Offs), Opt) :-
    offset_pos(Offs, Pos),
    seek(current_output, Pos, current, _),
    write_term(Term, Opt).
rportray('$NL', _) :- nl.
rportray('$PRIORITY'(T, Priority), Opt) :-
    integer(Priority), !,
    merge_options([priority(Priority)], Opt, Opt1),
    write_term(T, Opt1).
rportray([E|T1], Opt) :- !,
    append(H, T2, [E|T1]),
    ( var(T2)
    ->!, fail
    ; T2 = '$sb'(TermPos, IFrom, ITo, GTerm, GPriority, Term),
      is_list(Term),
      compound(TermPos), !,
      arg(1, TermPos, TFrom),
      arg(2, TermPos, TTo),
      term_innerpos(TFrom, TTo, From, To),
      T3 = '$sb'(From-To, IFrom, ITo, GTerm, GPriority, Term),
      with_output_to(string(SB), write_term(T3, Opt)),
      sub_string(SB, 1, _, 1, SC),
      refactor_context(text, Text),
      get_subtext(Text, TFrom, From, SL),
      get_subtext(Text, To, TTo, SR),
      format(atom(ST), "~s~s~s", [SL, SC, SR]),
      ( Term == []
      ->T = H,
        write('['),
        term_priority([_|_], user, 1, Priority),
        merge_options([priority(Priority)], Opt, Opt1),
        term_write_sep_list(T, ', ', Opt1),
        format("~s", [ST]),
        write(']')
      ; append(H, ['$TEXT'(ST)], T),
        write_term(T, Opt)
      )
    ).
% Better formatting:
rportray((:- Decl), Opt) :- !,
    write(':- '),
    merge_options([priority(1200)], Opt, Opt1),
    write_term(Decl, Opt1).
% Better formatting:
rportray(Term, OptL) :-
    callable(Term),
    \+ escape_term(Term),
    \+ current_arithmetic_function(Term),
    \+ ctrl(Term),
    Term \= (_:_),
    Term =.. [Name, Left, Right],
    option(module(M), OptL),
    \+ arithmetic:evaluable(Term, M),
    current_op(OptPri, Type, M:Name),
    valid_op_type_arity(Type, 2),
    !,
    option(priority(Pri), OptL),
    ( OptPri > Pri
    ->Display = yes
    ; Display = no
    ),
    term_priority_gnd(Term, M, 1, LP),
    merge_options([priority(LP)], OptL, OptL1),
    cond_display(Display, '('),
    write_term(Left, OptL1),
    write(' '),
    write(Name),
    write(' '),
    term_priority_gnd(Term, M, 2, RP),
    merge_options([priority(RP)], OptL, OptL2),
    write(''),
    write_term(Right, OptL2),
    cond_display(Display, ')').

pos_value(Pos, Value) :-
    ( rportray_pos(Pos, Value)
    ->true
    ; Pos == '$OUTPOS'
    ->get_output_position(Value)
    ; fail
    ).

arithexpression(X) :- number(X), !.
arithexpression(X) :-
    current_arithmetic_function(X),
    forall(arg(_, X, V), arithexpression(V)).

offset_pos(Offs, Pos) :-
    substitute(pos_value, Offs, Expr),
    arithexpression(Expr),
    catch(Pos is round(Expr), _, fail).

rportray_list_nl_b([], _, Opt) :- !, write_term([], Opt).
rportray_list_nl_b(L, Pos, Opt) :-
    write('['),
    rportray_list_nl_comma(L, Pos, Opt),
    write(']').

rportray_list_nl_comma(L, Pos, Opt) :-
    term_priority([_|_], user, 1, Priority),
    merge_options([priority(Priority)], Opt, Opt1),
    sep_nl(Pos, ',', Sep),
    rportray_list(L, write_term, Sep, Opt1).

rportray_list(L, Writter, Sep, Opt) :-
    deref_substitution(L, D),
    ( D = []
    ->true
    ; term_write_sep_list_2(D, Writter, Sep, Opt)
    ).

term_write_sep_list_2([E|T], Writter, Sep, Opt) :- !,
    call(Writter, E, Opt),
    term_write_sep_list_inner(T, Writter, Sep, Opt).
term_write_sep_list_2(E, Writter, _, Opt) :-
    call(Writter, E, Opt).

term_write_sep_list_inner(T, Writter, Sep, Opt) :-
    term_write_sep_list_inner_rec(T, Writter, Sep, Opt).

term_write_sep_list_inner_rec([E|T], Writter, SepIn, Opt) :- !,
    write(SepIn),
    call(Writter, E, Opt),
    term_write_sep_list_inner_rec(T, Writter, SepIn, Opt).
term_write_sep_list_inner_rec(T, Writter, SepIn, Opt) :-
    ( T == []
    ->true
    ; write_tail(T, Writter, SepIn, Opt)
    ).

term_write_sep_list([],    _,   _).
term_write_sep_list([T|L], Sep, Opt) :-
    write_term(T, Opt),
    maplist(term_write_sep_elem(Sep, Opt), L).

term_write_sep_elem(Sep, Opt, Term) :- write(Sep), write_term(Term, Opt).

term_write_comma_2(Opt, Term) :- write_term(Term, Opt), write(', ').

sep_nl(LinePos, Sep, SepNl) :-
    with_output_to(atom(In), line_pos(LinePos)),
    atomic_list_concat([Sep, '\n', In], SepNl).

write_tail(T, Writter, _, Opt) :-
    var(T), !,
    call(Writter, T, Opt).
write_tail([], _, _, _) :- !.
write_tail('$LIST,NL'(L), Writter, _, Opt) :- !,
    offset_pos('$OUTPOS', Pos),
    sep_nl(Pos, ',', Sep),
    term_write_sep_list_inner(L, Writter, Sep, Opt).
write_tail('$LIST,NL'(L, Offs), Writter, _, Opt) :-
    offset_pos(Offs, Pos), !,
    sep_nl(Pos, ',', Sep),
    term_write_sep_list_inner(L, Writter, Sep, Opt).
write_tail('$sb'(Pos1, IFrom, ITo, GTerm, GPriority, Term), Writter, SepIn, Opt) :-
    is_list(Term),
    nonvar(Pos1),
    arg(1, Pos1, From1),
    arg(2, Pos1, To1),
    !,
    refactor_context(text, Text),
    print_subtext(Text, From1, IFrom),
    ( Pos1 = list_position(_, _, PosL, Tail)
    ->write(SepIn),
      PosL = [LPos|_],
      arg(1, LPos, From),
      append(_, [RPos], PosL),
      ( Tail = none
      ->arg(2, RPos, To)
      ; arg(2, Tail, To)
      ),
      print_subtext_sb(Term, GTerm, list_position(From, To, PosL, Tail),
                         GPriority, Opt, Text)
    ; term_write_sep_list_inner_rec(Term, Writter, SepIn, Opt)
    ),
    print_subtext(Text, ITo, To1).
write_tail(T, Writter, _, Opt) :-
    write('|'),
    call(Writter, T, Opt).

print_expansion_rm_dot(TermPos, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, Before),
    sub_string(Text, Before, _, 0, Right),
    sub_string(Right, Next, _, _, "."),
    To is Before + Next + 2.

% Hacks that can only work at 1st level:

print_expansion_1('$RM', _, _, TermPos, _, Text, From, To) :-
    !,
    print_expansion_rm_dot(TermPos, Text, From, To).
print_expansion_1('$NODOT'(Into), Pattern, Term, TermPos, Options, Text, From, To) :-
    !,
    print_expansion_1(Into, Pattern, Term, TermPos, Options, Text, From, _To),
    print_expansion_rm_dot(TermPos, Text, _From, To).
print_expansion_1('$TEXT'(Into), _, _, TermPos, Options, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    % quoted(false)
    write_t(Into, Options).
print_expansion_1('$TEXT'(Into, Delta), _, _, TermPos, Options, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To1),
    % quoted(false)
    write_t(Into, Options),
    To is To1 + Delta.
print_expansion_1('$TEXTQ'(Into), _, _, TermPos, Options, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    write_q(Into, Options).
print_expansion_1('$TEXTQ'(Into, Delta), _, _, TermPos, Options, _, From, To) :- !,
    arg(1, TermPos, From),
    arg(2, TermPos, To1),
    write_q(Into, Options),
    To is To1 + Delta.
print_expansion_1('$LIST.NL'(IntoL), Pattern, Term, TermPos, Options1,
                  Text, From, To) :- !,
    merge_options([priority(1200)], Options1, Options),
    print_expansion_rm_dot(TermPos, Text, From, To),
    with_from(term_write_stop_nl_list(IntoL, Pattern, Term, TermPos, Options,
                                      Text), From).
print_expansion_1(Into, Pattern, Term, TermPos, Options, Text, From, To) :-
    print_expansion_2(Into, Pattern, Term, TermPos, Options, Text, From, To).

term_write_stop_nl_list([Into|IntoL], Pattern, Term, TermPos, Options, Text) :-
    term_write_stop_nl__(Into, Pattern, Term, TermPos, Options, Text),
    term_write_stop_nl_list(IntoL, Pattern, Term, TermPos, Options, Text).
term_write_stop_nl_list('$sb'(_, _, _, _, _, IntoL), Pattern, Term, TermPos,
                        Options, Text) :-
    term_write_stop_nl_list(IntoL, Pattern, Term, TermPos, Options, Text).
term_write_stop_nl_list([], _, _, _, _, _).

term_write_stop_nl__('$NOOP'(Into), Pattern, Term, TermPos, Options, Text) :- !,
    with_output_to(string(_),   %Ignore, but process
                   term_write_stop_nl__(Into, Pattern, Term, TermPos, Options,
                                        Text)).
term_write_stop_nl__('$NODOT'(Into), Pattern, Term, TermPos, Options, Text) :- !,
    print_expansion(Into, Pattern, Term, TermPos, Options, Text).
term_write_stop_nl__(Into, Pattern, Term, TermPos, Options, Text) :-
    print_expansion(Into, Pattern, Term, TermPos, Options, Text),
    write('.'), nl.

print_expansion_2(Into, Pattern, Term, TermPos, Options, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    with_from(print_expansion(Into, Pattern, Term, TermPos, Options, Text), From).

% if the term have been in parentheses, in a place where that was
% required, include it!!!
%
fix_position_if_braced(term_position(_, _, _, _, _), M,
                       Term, GPriority, Into, Priority, Display) :-
    ( \+ term_needs_braces(M:Term, GPriority),
      ( nonvar(Into),
        term_needs_braces(M:Into, Priority)
        % \+ term_needs_braces(M:Term, Priority)
      )
    ->Display=yes
    ),
    !.
fix_position_if_braced(_, _, _, _, _, _, no). % fail-safe

comp_priority(M, GTerm, GPriority, Term, Priority) :-
    \+ term_needs_braces(M:GTerm, GPriority),
    term_needs_braces(M:Term, Priority).

% :- meta_predicate term_needs_braces(:, +).
% If Term is a replacement, '$sb'/6, we assume that the substitution will not
% require braces (not sure if this is correct, but it works)
term_needs_braces(_:Term, _) :-
    \+ callable(Term),
    !,
    fail.
% term_needs_braces(M:'$sb'(_, _, _, _, _, Into), Pri) :- !,
%     term_needs_braces(M:Into, Pri).
term_needs_braces(M:Term, Pri) :-
    term_needs_braces(Term, M, Pri).

term_needs_braces(Term, M, Pri) :-
    functor(Term, Name, Arity),
    valid_op_type_arity(Type, Arity),
    current_op(OpPri, Type, M:Name),
    OpPri > Pri, !.

cond_display(yes, A) :- write(A).
cond_display(no,  _).

:- meta_predicate
       with_cond_braces(5, +, +, +, +, +, +).

print_expansion_sb(Into, Pattern, GTerm, TermPos, GPriority, Options, Text) :-
    with_cond_braces(do_print_expansion_sb(Pattern), Into, GTerm, TermPos, GPriority, Options, Text).

do_print_expansion_sb(Pattern, Into, GTerm, TermPos, Options, Text) :-
    arg(1, TermPos, From),
    with_from(print_expansion_ne(Into, Pattern, GTerm, TermPos, Options, Text), From).

print_subtext_sb(Into, GTerm, TermPos, GPriority, Options, Text) :-
    with_cond_braces(print_subtext, Into, GTerm, TermPos, GPriority, Options, Text).

print_subtext(_, Term, TermPos, Options, Text) :-
    get_subtext(TermPos, Text, SubText),
    ( sub_string(SubText, 0, 1, _, C),
      char_type(C, space)
    ->true
    ; fix_if_partial_term(Term, _, Options)
    ),
    ( sub_string(SubText, _, 1, 0, E),
      char_type(E, space)
    ->write('')
    ; true
    ),
    format("~s", [SubText]).

fix_if_partial_term(Term, Offset, Options) :-
    fix_if_partial_term_(Term, Offset, Options).

fix_if_partial_term_(Term, Offset, Options) :-
    nonvar(Term),
    Term = '$sb'(_, _, _, GTerm, _, _), !,
    fix_if_partial_term_(GTerm, Offset, Options).
fix_if_partial_term_(Term, Offset, Options) :-
    ( nonvar(Term)
    ->functor(Term, F, A),
      functor(Func, F, A),
      numbervars(Func, 0, _),
      select_option(portray_goal(_), Options, Options2),
      with_output_to(
          string(_),
          ( stream_property(current_output, position(SPos1)),
            write_term(Func, Options2),
            stream_property(current_output, position(SPos2))
          )),
      stream_property(current_output, position(Pos1)),
      write_term(Func, Options2),
      stream_property(current_output, position(Pos2)),
      stream_position_data(char_count, Pos1, B1),
      stream_position_data(char_count, Pos2, B2),
      stream_position_data(char_count, SPos1, S1),
      stream_position_data(char_count, SPos2, S2),
      Offset is B2-B1+S1-S2,
      write(''), % kludge to reset partial(true) logic
      set_stream_position(current_output, Pos1),
      seek(current_output, Offset, current, _)
    ; Offset = 0
    ).

with_cond_braces(Call, Into, GTerm, TermPos, GPriority, Options, Text) :-
    option(module(M), Options),
    option(priority(Priority), Options),
    fix_position_if_braced(TermPos, M, GTerm, GPriority, Into, Priority, Display),
    cond_display(Display, '('),
    call(Call, Into, GTerm, TermPos, Options, Text),
    cond_display(Display, ')').

% TODO: stream position would be biased --EMM
with_str_hook(Command, StrHook) :-
    with_output_to(string(S1), call(Command)),
    ( call(StrHook, S1, S)
    ->true
    ; S = S1
    ),
    format('~s', [S]).

%!  print_expansion(?Into:term, ?Pattern:term, ?Term:Term, RefPos, Priority:integer, Options:list, Text:string) is det
%
print_expansion(Var, _, _, RefPos, Options, Text) :-
    var(Var),
    !,
    option(variable_names(VNL), Options, []),
    ( member(Name=Var1, VNL),
      Var1==Var
    ->write(Name)
    ; print_subtext(RefPos, Text)
    ).
print_expansion('$sb'(RefPos), _, Term, _, _, Text) :-
    \+ ( nonvar(Term),
         Term = '$sb'(_)
       ),
    !,
    print_subtext(RefPos, Text).
print_expansion('$sb'(RefPos, _IFrom, _ITo, GTerm, Priority, Into), _Pattern, Term, _RPos, Options, Text) :-
    nonvar(RefPos),
    \+ ( nonvar(Term),
         Term = '$sb'(_, _, _, _, _, _),
         Into \= '$sb'(_, _, _, _, _, _)
       ),
    !,
    print_subtext_sb(Into, GTerm, RefPos, Priority, Options, Text).
print_expansion(Into, Pattern, Term, RefPos, Options, Text) :-
    print_expansion_ne(Into, Pattern, Term, RefPos, Options, Text).

print_expansion_ne('$G'(Into, Goal), Pattern, Term, RefPos, Options, Text) :-
    \+ ( nonvar(Term),
         Term = '$G'(_, _)
       ),
    !,
    with_str_hook(print_expansion(Into, Pattern, Term, RefPos, Options, Text),
                  Goal).
print_expansion_ne('$C'(Goal, Into), Pattern, Term, RefPos, Options, Text) :-
    \+ ( nonvar(Term),
         Term = '$C'(_, _)
       ),
    !,
    call(Goal),
    print_expansion(Into, Pattern, Term, RefPos, Options, Text).
print_expansion_ne('$,NL', Pattern, Term, RefPos, Options, Text) :-
    Term \=='$,NL',
    !,
    %% Print a comma + indented new line
    write(','),
    print_expansion('$NL', Pattern, Term, RefPos, Options, Text).
print_expansion_ne('$NL', _, Term, _, _, Text) :- % Print an indented new line
    Term \== '$NL',
    !,
    refactor_context(from, From),
    textpos_line(Text, From, _, LinePos),
    nl,
    line_pos(LinePos).
print_expansion_ne(Into, SPattern, Term1, _, Options, Text) :-
    nonvar(SPattern),
    nonvar(Term1),
    Term1\='$sb'(_, _, _, _, _, _), % is not a read term, but a command
    SPattern='$sb'(RefPos, _, _, Term, _, Pattern),
    !,
    print_expansion_ne(Into, Pattern, Term, RefPos, Options, Text).
print_expansion_ne(Into, Pattern, Term, RefPos, Options, Text) :-
    ( \+ escape_term(Into),
      print_expansion_pos(RefPos, Into, Pattern, Term, Options, Text)
    ->true
    ; write_term(Into, Options)
    ).

print_expansion_arg(M, MInto, Options1, Text, Length, I, FromTo,
                    v(N, RefPos, Into, Pattern, GTerm), Freeze1, Freeze) :-
    term_priority(MInto, M, N, Priority),
    merge_options([priority(Priority)], Options1, Options),
    print_expansion_elem(Options, Text, Length, I, FromTo, RefPos, Into, Pattern-GTerm, Freeze1, Freeze).

print_expansion_elem(Options, Text, Length, I, From-To, RefPos, Into, Pattern-GTerm, Freeze1, Freeze) :-
    ( Into == '$RM',
      GTerm \== '$RM'
    ->( Length = I
      ->Freeze = true
      ; Freeze = Freeze1
      )
    ; Freeze1 = true,
      print_expansion(Into, Pattern, GTerm, RefPos, Options, Text),
      freeze(Freeze, print_subtext(Text, From, To))
    ).

escape_term($@(_)).
escape_term(\\(_)).
escape_term(_@@_).
escape_term(_$@_).
% escape_term('$G'(_, _)).
% escape_term('$C'(_, _)).
escape_term('$NOOP'(_)).
escape_term('$NODOT'(_)).
escape_term('$LIST'(_)).
escape_term('$LISTC'(_)).
escape_term('$LIST,'(_)).
escape_term('$LIST,_'(_)).
escape_term('$LIST,NL'(_)).
escape_term('$LIST,NL'(_, _)).
escape_term('$NL'(_, _)).
escape_term('$POS'(_, _)).
escape_term('$SEEK'(_, _)).
escape_term('$LISTC.NL'(_)).
escape_term('$LISTB,NL'(_)).
escape_term('$LISTB,NL'(_, _)).
escape_term('$PRIORITY'(_, _)).
escape_term('$TEXT'(_)).
escape_term('$TEXT'(_, _)).
escape_term('$TEXTQ'(_)).
escape_term('$TEXTQ'(_, _)).
escape_term('$CLAUSE'(_)).
escape_term('$CLAUSE'(_, _)).
escape_term('$BODY'(_, _)).
escape_term('$BODY'(_)).
escape_term('$BODYB'(_, _)).
escape_term('$BODYB'(_)).

valid_op_type_arity(xf,  1).
valid_op_type_arity(yf,  1).
valid_op_type_arity(xfx, 2).
valid_op_type_arity(xfy, 2).
valid_op_type_arity(yfx, 2).
valid_op_type_arity(fy,  1).
valid_op_type_arity(fx,  1).

from_to_pairs([], _, To, To) --> [].
from_to_pairs([To2-From2|PosL], From1, To1, To) -->
    { (To2   = 0 -> To1  = From1 ; To1  = To2),
      (From2 = 0 -> From = To1   ; From = From2)
    },
    [From-To3],
    from_to_pairs(PosL, From, To3, To).

normalize_pos(Pos, F-T) :-
    arg(1, Pos, F),
    arg(2, Pos, T).

print_expansion_pos(term_position(From, To, FFrom, FFTo, PosT),
                    Into, Pattern, GTerm, Options, Text) :-
    compound(Into),
    Into \= [_|_],
    functor(Into,    FT, A),
    nonvar(Pattern),
    functor(Pattern, FP, A),
    option(module(M), Options),
    ( FT == FP
    ->NT = FT % preserve layout
    ; NT = '$TEXTQ'(FT),
      ( option(priority(Priority), Options),
        current_op(PrP, TypeOpP, M:FP),
        valid_op_type_arity(TypeOpP, A),
        current_op(PrT, TypeOpT, M:FT),
        valid_op_type_arity(TypeOpT, A),
        PrT =< Priority,
        ( PrP =< PrT
        ; forall(arg(AP, Into, Arg),
                 ( term_priority_gnd(Into, M, AP, PrA),
                   \+ term_needs_braces(M:Arg, PrA)))
        )
      ; option(module(M), Options),
        \+ current_op(_, _, M:FT),
        \+ current_op(_, _, M:FP)
      )
    ), !,
    mapnlist([Into, Pattern, GTerm] +\ N^Pos^(PosK-v(N, Pos, Arg, PAr, GAr))^
            ( arg(N, Into, Arg),
              arg(N, Pattern, PAr),
              arg(N, GTerm, GAr),
              normalize_pos(Pos, PosK)
            ), 1, PosT, KPosValTU),
    /* 0 is the functor, priority 1200 */
    KPosValU = [(FFrom-FFTo)-v(0, FFrom-FFTo, NT, FP, FP)|KPosValTU],
    keysort(KPosValU, KPosValL),
    pairs_keys_values(KPosValL, PosKL, ValL),
    from_to_pairs(PosKL, From, To1, To2, FromToL, []),
    succ(A, N),
    nth1(N, PosKL, E),
    arg(2, E, To2),
    print_subtext(Text, From, To1),
    foldnl(print_expansion_arg(M, Into, Options, Text, A), 1, FromToL, ValL, _, true),
    print_subtext(Text, To2, To).
print_expansion_pos(list_position(From, To, PosL, PosT), Into, Pattern, GTerm, Options1, Text) :-
    maplist(normalize_pos, PosL, PosN),
    from_to_pairs(PosN, From, To1, To2, FromToL, []),
    length(PosL, N),
    ( trim_list(N, Into, ArgL, ATail)
    ->Delta = 0
    % ; length(LTerm, N),               % Special case: list converted to sequence
    %   once(list_sequence(LTerm, Into)),
    %   trim_list(N, LTerm, ArgL, ATail),
    %   Delta = 1
    ),
    trim_list(N, Pattern, PatL, PTail),
    trim_list(N, GTerm,   GTrL, GTail),
    pairs_keys_values(PatGTrL, PatL, GTrL),
    !,
    From1 is From + Delta,
    option(module(M), Options1),
    term_priority(Into, M, 1, Priority1),
    select_option(priority(Priority), Options1, Options, Priority),
    Options2=[priority(Priority1)|Options],
    ( comp_priority(M, GTerm, Priority, Into, Priority)
    ->write('(')
    ; Delta = 1
    ->write(' ')    % Only if [...] ---> (...)
    ; true
    ),
    print_subtext(Text, From1, To1),
    ( PosT \= none
    ->arg(1, PosT, PTo),
      term_priority(Into, M, 2, Priority2),
      To2 is PTo + Delta,
      foldnl(print_expansion_elem(Options2, Text, N), 1, FromToL, PosL, ArgL, PatGTrL, _, true),
      arg(2, PosT, PFrom),
      Options3=[priority(Priority2)|Options],
      print_expansion_elem(Options3, Text, 1, 1, PFrom-To, PosT, ATail, PTail-GTail, _, true)
    ; nth1(N, PosL, E),
      arg(2, E, To2),
      foldnl(print_expansion_elem(Options2, Text, N), 1, FromToL, PosL, ArgL, PatGTrL, _, _),
      term_priority(Into, M, 2, Priority2),
      Options3=[priority(Priority2)|Options],
      term_write_sep_list_inner_rec(ATail, write_term, ', ', Options3),
      print_subtext(Text, To2, To)
    ),
    ( comp_priority(M, GTerm, Priority, Into, Priority)
    ->write(')')
    ; true
    ).
print_expansion_pos(brace_term_position(From, To, TermPos), {Into}, {Pattern},
                    {GTerm}, Options, Text) :-
    arg(1, TermPos, AFrom),
    arg(2, TermPos, ATo),
    print_subtext(Text, From, AFrom),
    option(module(M), Options),
    print_expansion_arg(M, {Into}, Options, Text, 1, 1, ATo-To, v(1, TermPos, Into, Pattern, GTerm), _, true).
print_expansion_pos(parentheses_term_position(From, To, TermPos), Into, Pattern,
                    GTerm, Options1, Text) :-
    arg(1, TermPos, AFrom),
    arg(2, TermPos, ATo),
    print_subtext(Text, From, AFrom),
    merge_options([priority(1200)], Options1, Options),
    print_expansion_elem(Options, Text, 1, 1, ATo-To, TermPos, Into, Pattern-GTerm, _, true).
print_expansion_pos(TermPos, Into, _Pattern, GTerm, _, Text) :-
    Into==GTerm,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    print_subtext(Text, From, To).

print_subtext(RefPos, Text) :-
    get_subtext(RefPos, Text, SubText),
    print_text(SubText).

print_text(Text) :-
    format("~s", [Text]),
    write(''). % reset partial(true) logic

trim_list(N, L1, L, T) :-
    length(L, N),
    append(L, T, L1).

print_subtext(Text, From, To) :-
    get_subtext(Text, From, To, SubText),
    print_text(SubText).

get_subtext(RefPos, Text, SubText) :-
    arg(1, RefPos, From),
    arg(2, RefPos, To),
    get_subtext(Text, From, To, SubText).

get_subtext(Text1, From, To, Text) :-
    LPaste is To - From,
    sub_string(Text1, From, LPaste, _, Text).

bin_op(Term, Op, Left, Right, A, B) :-
    nonvar(Term),
    functor(Term, Op, N),
    N == 2,
    prolog_listing:infix_op(Op, Left, Right),
    arg(1, Term, A),
    arg(2, Term, B).

rportray_bodyb(B, Pos, OptL) :-
    fix_if_partial_term(B, D, OptL),
    write_b(B, OptL, Pos+D).

rportray_body(B, Pos, OptL) :-
    fix_if_partial_term(B, D, OptL),
    write_b1(B, OptL, Pos+D).

write_b(Term, OptL, Pos1) :-
    ( option(priority(N), OptL),
      option(module(M), OptL),
      term_needs_braces(M:Term, N)
    ->write('( '),
      Pos is Pos1 + 2,
      write_b1(Term, OptL, Pos),
      nl,
      line_pos(Pos1),
      write(')')
    ; write_b1(Term, OptL, Pos1)
    ).

and_layout(T) :- T = (_,_).

write_b1(Term, OptL, Pos) :-
    prolog_listing:or_layout(Term), !,
    write_b_layout(Term, OptL, or,  Pos).
write_b1(Term, OptL, Pos) :-
    and_layout(Term), !,
    write_b_layout(Term, OptL, and, Pos).
write_b1(Term, OptL, _Pos) :-
    option(module(M), OptL),
    ( nonvar(Term),
      has_meta(Term, M, 0, Spec)
    ->body_meta_args(Term, Spec, TMeta)
    ; TMeta = Term
    ),
    write_term(TMeta, OptL).

has_meta(Term, _, _, _) :-
    var(Term), !, fail.
has_meta(M:Term, _, Meta, Spec) :- !,
    has_meta(Term, M, Meta, Spec).
has_meta(Term, M, Meta, Spec) :-
    \+ memberchk(Term, ['$BODYB'(_),
                        '$BODYB'(_, _)]),
    predicate_property(M:Term, meta_predicate(Spec)),
    ( findall(Arg,
              ( arg(Idx, Spec, Meta),
                arg(Idx, Term, Arg),
                nonvar(Arg)
              ), ArgL),
      ( ArgL = [_, _, _|_]
      ; member(Arg, ArgL),
        has_meta(Arg, M, 0, _)
      )
    ->true
    ; ctrl(Term)
    ).

body_meta_args(Term, Spec, Meta) :-
    functor(Term, F, N),
    functor(Meta, F, N),
    stream_property(current_output, position(Pos)),
    ID='$body_meta_args'(Pos), % Trick to get a unique ID
    mapnargs(body_meta_arg(ID, Arg1, Meta1), Term, Spec, Meta),
    ( var(Meta1) -> Meta1 = Arg1 ; true ).

ctrl((_ ,   _)).
ctrl((_ ;   _)).
ctrl((_ ->  _)).
ctrl((_ *-> _)).

body_meta_arg(ID, Arg1, Meta1, I, Term, Spec, Meta) :-
    ( Spec = 0,
      nonvar(Term)
    ->( I \= 1
      ->Meta = '$SEEK'('$NL'('$BODYB'(Term), ID), -1),
        Meta1 = '$POS'(ID, Arg1)
      ; Meta1 = Meta,
        Arg1 = '$BODYB'(Term)
      )
    ; ( I = 1
      ->Meta1 = Meta,
        Arg1 = Term
      ; Meta = Term
      )
    ).

write_b_layout(Term, OptL1, Layout, Pos) :-
    bin_op(Term, Op, Left, Right, A, B),
    !,
    merge_options([priority(Left)], OptL1, OptL2),
    write_b(A, OptL2, Pos),
    nl_indent(Layout, Op, Pos),
    merge_options([priority(Right)], OptL1, OptL3),
    write_b(B, OptL3, Pos).

nl_indent(or, Op, LinePos) :-
    nl,
    line_pos(LinePos - 2),
    format(atom(A), '~|~a~2+',[Op]),
    % Kludge to reset logic of partial(true):
    write(A).
nl_indent(and, Op, LinePos) :-
    writeln(Op),
    line_pos(LinePos).

line_pos(LinePos) :-
    setting(listing:tab_distance, N),
    N > 0,
    LinePos >= N,
    !,
    write('\t'),
    LinePos1 is LinePos - N,
    line_pos(LinePos1).
line_pos(LinePos) :-
    LinePos > 0,
    !,
    write(' '),
    write(''),
    LinePos1 is LinePos - 1,
    line_pos(LinePos1).
line_pos(_).

write_t(Term, Options1) :-
    merge_options([quoted(false), priority(1200)], Options1, Options),
    write_term(Term, Options).

write_q(Term, Options1) :-
    merge_options([quoted(true), priority(1200)], Options1, Options),
    write_term(Term, Options).
