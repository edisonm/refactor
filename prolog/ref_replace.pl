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

:- module(ref_replace,
          [replace/5,
           op(100,xfy,($@)),
           op(100,xfy,(@@))
          ]).

/** <module> Basic Term Expansion operations

  This library provides the predicate replace/5, which is the basic entry point
  for all the refactoring scenarios.

  Note for implementors/hackers:

  * Be careful with some variables, they use destructive assignment --TODO:
    document them.

  * format("~a", [Atom]) does not behaves as write_term(Atom, Options), since a
    space is not added to separate operators from the next term, for instance
    after rewriting :- dynamic a/1, you would get :- dynamica/1.

  * write('') is used to reset the effect of the partial(true) option

*/

:- use_module(library(apply)).
:- use_module(library(codesio)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(settings)).
:- use_module(library(solution_sequences)).
:- use_module(library(neck)).
:- use_module(library(foldil)).
:- use_module(library(term_size)).
:- use_module(library(prolog_source), []). % expand/4
:- use_module(library(readutil)).
:- use_module(library(fix_termpos)).
:- use_module(library(mapnargs)).
:- use_module(library(ref_changes)).
:- use_module(library(ref_context)).
:- use_module(library(ref_msgtype)).
:- use_module(library(ref_message)).
:- use_module(library(term_info)).
:- use_module(library(sequence_list)).
:- use_module(library(clambda)).
:- use_module(library(mapilist)).
:- use_module(library(linearize)).
:- use_module(library(substitute)).
:- use_module(library(subpos_utils)).
:- use_module(library(transpose)).
:- use_module(library(option_utils)).

:- thread_local
    command_db/1.

:- multifile
    prolog:xref_open_source/2.  % +SourceId, -Stream

:- thread_local
    rportray_pos/2,
    ref_position/3,
    rportray_skip/0.

:- meta_predicate
    apply_commands(?, +, +, ?, +, +, +, 4),
    fixpoint_file(+, +, 0),
    replace(+,?,?,0,:),
    rportray_list(+, 2, +, +),
    with_context(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, ?),
    with_cond_braces_2(4, ?, ?, ?, ?, ?, ?),
    with_counters(0, +),
    with_styles(0, +).

%!  replace(+Level, +Pattern, -Into, :Expander, :Options) is det
%
%   Given a Level of operation, in all terms of the source code that subsumes
%   Pattern, replace each Pattern with Into, provided that Expander succeeds.
%   Expander can be used to finalize the shape of Into as well as to veto the
%   expansion (if fails). The Options argument is used to control the behavior
%   and scope of the replacement.
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
%   * '$APP'(L1, L2)
%     Print the result of append(L1, L2, L), but preserving the formats of L1 and L2
%     Note that if you use append/3 directly, the format of L1 will be lost
%
%   * '$LISTC'(L)
%     Print each element of L in a way similar to portray_clause
%
%   * '$LISTC.NL'(L)
%     Print each element of L in a way similar to portray_clause followed by a
%     dot and a new line.  If Level is sent, the tool will add this
%     automatically if the replacement is a list, and in the case of an empty
%     list, the sentence will be removed.
%
%   * '$LIST,'(L)
%     Print each element of L placing a comma between them
%
%   * '$LIST,_'(L)
%     Print each element of L followed by a comma
%
%   * '$LIST,NL'(L)
%     Print each element of L followed by a comma and a new line
%
%   * '$LISTNL'(L)
%     Print each element of L followed by a new line.
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
%     Seek O in the current output before to print T.
%
%   * '$TAB'(T, O)
%     Print as many spaces as needed to make O the current write position
%
%   Specific options for this predicate are:
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
%
%   * line(-Line)
%     Unifies Line with the line number of the sentence being refactorized.
%
%   * clause(+Ref)
%     Apply the refactoring to the clause refered by Ref.
%
%   * max_tries(MaxTries)
%     Apply no more than MaxTries changes
%
%   * syntax_errors(SE)
%     Default error
%
%   * subterm_positions(SentPos)
%
%   * term_position(Pos)
%
%   * conj_width(+ConjWidth)
%     Print several conjunctions in the same line, provided that they don't
%     surpasses ConjWidth columns.
%     Default is 120
%
%   * term_width(+TermWidth)
%     Split long terms so that when printed, they don't surpasses TermWidth
%     columns.
%     Default is 120
%
%   * list_width(+ListWidth)
%     Split long lists so that when printed, they don't surpasses ListWidth
%     columns.
%     Default is 120
%
%   * linearize(+Linearize)
%     Linearize is a subset of [vars, atms], which will linearize the term to
%     avoid bounded variables or atoms.  In some refactoring scenarios this is
%     important if we want to avoid ambiguities.  For instance, supose that you
%     want to replayce f(A, B), by f(B, A), but if one of the matching terms is
%     f(X, X), the change will not be performed, even if the two arguments have
%     different layouts.  To avoid we should use the option linearize([term]).
%     Default is [].
%
%   * sentence(-SentPattern)
%     Unifies SentPattern with the Sentence being processed.  This is useful in
%     some refactoring scenarios.
%
%   * comments(-Comments)
%     Passed to read_term/2 to get the list of Comments.
%
%   * expand(Expand)
%
%   * expanded(Expanded)
%
%   * cleanup_attributes(CleanupAttributes)
%     Default yes
%
%   * fixpoint(FixPoint)
%     Default decreasing
%
%   * max_changes(Max)
%
%   * variable_names(VNL)
%
%   * vars_preffix(Preffix)
%     Default 'V'
%
%   * file(AFile)
%
%   * if(Loaded)
%     if Loaded is true (default), refactor non loaded files too
%
%   * subterm_boundary(+Boundary)
%     Processed by fix_termpos/2 to stablish the boundaries of the subterms.
%
%   Other options are processed by the predicate option_module_files/2 and allows
%   to select the files or modules that are going to be modified.

replace(Level, Patt, Into, Expander, MOptions) :-
    %% At this point we are not interested in styles
    meta_options(replace_meta_option, MOptions, Options),
    with_styles(with_counters(do_replace(Level, Patt, Into, Expander, Options),
                              Options), [-singleton]).

replace_meta_option(decrease_metric).

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

% Note: To avoid this hook be applied more than once, we record the positions
% already refactorized in ref_position/3.

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
    with_varnames(
        forall(substitute_term_norec(sub, M, Term, TermPos, 999, data(Pattern, Into, Expander, TermPos), Command),
               assertz(command_db(Command))),
        VNL).

do_replace(Level, Patt, Into, Expander, Options) :-
    setup_call_cleanup(
        prepare_level(Level, Ref),
        apply_ec_term_level(Level, Patt, Into, Expander, Options),
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

with_counters(Goal, Options1) :-
    foldl(select_option_default,
          [max_tries(MaxTries)-MaxTries],
          Options1, Options),
    with_refactor_context(
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
        [max_tries],
        [MaxTries]
    ).

param_module_file(clause(CRef), M, File) :-
    clause_property(CRef, file(File)),
    clause_property(CRef, module(M)).
param_module_file(mfiled(MFileD), M, File) :-
    get_dict(M1, MFileD, FileD),
    ( M1 = (-)
    ->true
    ; M = M1
    ),
    get_dict(File, FileD, _).

apply_ec_term_level(Level, Patt, Into, Expander, Options1) :-
    (Level = goal -> DExpand=yes ; DExpand = no),
    (Level = sent -> SentPattern = Patt ; true), % speed up
    option(module(M), Options1, M),
    foldl(select_option_default,
          [max_tries(MaxTries)-MaxTries,
           syntax_errors(SE)-error,
           subterm_positions(SentPos)-SentPos,
           term_position(Pos)-Pos,
           conj_width(ConjWidth)-120, % In (_,_), try to wrap lines
           term_width(TermWidth)-120, % In terms, try to wrap lines
           list_width(ListWidth)-120, % In lists, try to wrap lines
           linearize(Linearize)-[],
           sentence(SentPattern)-SentPattern,
           comments(Comments)-Comments,
           expand(Expand)-DExpand,
           expanded(Expanded)-Expanded,
           cleanup_attributes(CleanupAttributes)-yes,
           fixpoint(FixPoint)-decreasing,
           max_changes(Max)-Max,
           variable_names(VNL)-VNL,
           vars_preffix(Preffix)-'V',
           file(AFile)-AFile,
            % By default refactor even non loaded files
           if(Loaded)-true
          ],
          Options1, Options2),
    ( option(clause(CRef), Options2)
    ->MFileParam = clause(CRef),
      clause_property(CRef, line_count(Line)),
      merge_options([line(Line)], Options2, Options3)
    ; option_module_files([if(Loaded), file(AFile)|Options2], MFileD),
      MFileParam = mfiled(MFileD),
      Options3 = Options2
    ),
    Options = [syntax_errors(SE),
               subterm_positions(SentPos),
               variable_names(VNL),
               conj_width(ConjWidth),
               term_width(TermWidth),
               list_width(ListWidth),
               comments(Comments)|Options3],
    ignore(( var(AFile),
             File = AFile
           )),
    setup_call_cleanup(
        ( '$current_source_module'(OldM),
          freeze(M, '$set_source_module'(_, M))
        ),
        process_sentences(
            MFileParam, FixPoint, Max, SentPattern, Options, CleanupAttributes, M, File, Expanded, Expand, Pos,
            ga(Patt, Into, Expander), Linearize, MaxTries, Preffix, Level, data(Patt, Into, Expander, SentPos)),
        '$set_source_module'(_, OldM)).

:- use_module(library(countsols)).
:- use_module(library(conc_forall)).

param_module_file_sorted(MFileParam, M, File) :-
    order_by([desc(Size)],
             ( param_module_file(MFileParam, M, File),
               ignore(size_file(File, Size))
             )).

process_sentences(
    MFileParam, FixPoint, Max, SentPattern, Options, CleanupAttributes, M, File, Expanded, Expand,
    Pos, GoalArgs, Linearize, MaxTries, Preffix, Level, Data) :-
    index_change(Index),
    ini_counter(0, STries),
    ini_counter(0, SCount),
    conc_forall(
        param_module_file_sorted(MFileParam, M, File),
        process_sentence_file(
            Index, FixPoint, Max, SentPattern, Options, CleanupAttributes,
            M, File, Expanded, Expand, Pos, GoalArgs, Linearize, MaxTries,
            Preffix, Level, Data, Tries, Count),
        ( inc_counter(STries, Tries, _),
          inc_counter(SCount, Count, _)
        )),
    STries = count(Tries),
    SCount = count(Count),
    set_refactor_context(tries, Tries),
    set_refactor_context(count, Count).

fixpoint_file(none, _, Goal) :- ignore(Goal).
fixpoint_file(true, Max, Goal) :-
    repeat,
      set_refactor_context(modified, false),
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

process_sentence_file(Index, FixPoint, Max, SentPattern, Options, CleanupAttributes,
                      M, File, Expanded, Expand, Pos, GoalArgs,
                      Linearize, MaxTries, Preffix, Level, Data, Tries, Count) :-
    maplist(set_refactor_context,
            [bindings, cleanup_attributes, comments, expanded, file, goal_args, modified,
             tries, count, max_tries, options, pos, preffix, sent_pattern, sentence, subpos],
            [Bindings, CleanupAttributes,  Comments, Expanded, File, GoalArgs,  false,
             0,     0,     MaxTries,  Options, Pos, Preffix, SentPattern,  Sent,     SentPos]),
    \+ \+ ( option(comments(Comments),  Options, Comments),
            option(subterm_positions(SentPos), Options, SentPos),
            option(variable_names(VNL), Options, VNL),
            level_rec(Level, Rec),
            rec_fixpoint_file(Rec, FixPoint, FPFile),
            fixpoint_file(FPFile, Max,
                          apply_commands(
                              Index, File, Level, M, Rec, FixPoint, Max,
                              gen_module_command(
                                  SentPattern, Options, Expand, SentPos, Expanded,
                                  Linearize, Sent, VNL, Bindings, Data)))
          ),
    refactor_context(tries, Tries),
    refactor_context(count, Count).

binding_varname(VNL, Var=Term) -->
    ( { atomic(Term),
        Term \= [],
        atomic_concat('_Atm_', Term, Name)
      ; member(Name=Var1, VNL),
        Var1==Term
      }
    ->[Name=Var]
    ; []
    ).

gen_module_command(SentPattern, Options, Expand, SentPos, Expanded, Linearize,
                   Sent, VNL, Bindings, Data, Level, M, Cmd, In) :-
    ref_fetch_term_info(SentPattern, RawSent, Options, In, Once),
    b_setval('$variable_names', VNL),
    expand_if_required(Expand, M, RawSent, SentPos, In, Expanded),
    make_linear_if_required(RawSent, Linearize, Sent, Bindings),
    foldl(binding_varname(VNL), Bindings, RVNL, VNL),
    S = solved(no),
    ( true
    ; arg(1, S, yes)
    ->cond_cut_once(Once),
      fail
    ),
    set_refactor_context(variable_names, RVNL),
    substitute_term_level(Level, M, Sent, SentPos, 1200, Data, Cmd),
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
    option(subterm_positions(Size-Size), Options),
    option(variable_names([]), Options).
ref_term_info_file([], [], Options, _) :-
    option(subterm_positions(0-0), Options),
    option(variable_names([]), Options).

expand_if_required(Expand, M, Sent, SentPos, In, Expanded) :-
    ( Expand = no
    ->Expanded = Sent
    ; '$expand':expand_terms(prolog_source:expand, Sent, SentPos, In, Expanded)
    ),
    ignore(( '$set_source_module'(CM, CM),
             M = CM
           )),
    prolog_source:update_state(Sent, Expanded, M).

make_linear_if_required(Sent, Linearize, Linear, Bindings) :-
    foldl(linearize, Linearize, Sent-Bindings, Linear-[]).

linearize(Which, Sent-Bindings1, Linear-Bindings) :-
    linearize(Which, Sent, Linear, Bindings1, Bindings).

prolog:xref_open_source(File, Fd) :-
    nb_current(ti_open_source, yes),
    !,
    ( pending_change(_, File, Text)
    ->true
    ; read_file_to_string(File, Text, [])
    ),
    open_codes_stream(Text, Fd).
    % set_refactor_context(text, Text). % NOTE: update_state/2 has the side effect of
                                     % modify refactor_text

substitute_term_level(goal, _, _, _, _, _, Cmd) :-
    retract(command_db(Cmd)).
substitute_term_level(term, M, Sent, SentPos, Priority, Data, Cmd) :-
    substitute_term_rec(M, Sent, SentPos, Priority, Data, Cmd).
substitute_term_level(sent, M, Sent, SentPos, Priority, Data, Cmd) :-
    substitute_term_norec(top, M, Sent, SentPos, Priority, Data, Cmd).
substitute_term_level(head, M, Sent, SentPos, Priority, Data, Cmd) :-
    substitute_term_head(norec, M, Sent, SentPos, Priority, Data, Cmd).
substitute_term_level(head_rec, M, Sent, SentPos, Priority, Data, Cmd) :-
    substitute_term_head(rec, M, Sent, SentPos, Priority, Data, Cmd).
substitute_term_level(body, M, Sent, SentPos, _, Data, Cmd) :-
    substitute_term_body(norec, M, Sent, SentPos, Data, Cmd).
substitute_term_level(body_rec, M, Sent, SentPos, _, Data, Cmd) :-
    substitute_term_body(rec, M, Sent, SentPos, Data, Cmd).

substitute_term_body(Rec, M, Sent, parentheses_term_position(_, _, TermPos), Data, Cmd) :- !,
    substitute_term_body(Rec, M, Sent, TermPos, Data, Cmd).
substitute_term_body(Rec, M, (_ :- Body), term_position(_, _, _, _, [_, BodyPos]), Data,
                     Cmd) :-
    term_priority((_ :- Body), M, 2, Priority),
    substitute_term(Rec, sub, M, Body, BodyPos, Priority, Data, Cmd).

substitute_term_head(Rec, M, Clause, parentheses_term_position(_, _, TermPos), Priority,
                     Data, Cmd) :- !,
    substitute_term_head(Rec, M, Clause, TermPos, Priority, Data, Cmd).
substitute_term_head(Rec, M, Clause, TermPos, Priority, Data, Cmd) :-
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
    substitute_term(Rec, sub, M, Head, HeadPos, HPriority, Data, Cmd).

mhead_pos(parentheses_term_position(_, _, Pos), HPos) :- !, mhead_pos(Pos, HPos).
mhead_pos(term_position(_, _, _, _, [_, HPos]), HPos).

substitute_term(rec, _, M, Term, TermPos, Priority, Data, Cmd) :-
    substitute_term_rec(M, Term, TermPos, Priority, Data, Cmd).
substitute_term(norec, Level, M, Term, TermPos, Priority, Data, Cmd) :-
    substitute_term_norec(Level, M, Term, TermPos, Priority, Data, Cmd).

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
rec_ft(false,      not).

% This is weird due to the operators
apply_commands(Index, File, Level, M, Rec, FixPoint, Max, GenMCmd) :-
    ( pending_change(_, File, Text1)
    ->true
    ; exists_file(File)
    ->read_file_to_string(File, Text1, [])
    ; Text1 = ""
    ),
    rec_fixpoint_term(Rec, FixPoint, FPTerm),
    with_refactor_context(
        with_source_file(
            File, In,
            apply_commands_stream(
                FPTerm, GenMCmd, File, Level, M, nocs, Max, In, Text1, Text)),
        [text, file], [Text1, File]),
        ( Text1 \= Text
        ->nb_set_refactor_context(modified, true),
          save_change(Index, File-Text)
        ; true
        ).

decreasing_recursion(nocs, _).
decreasing_recursion(subst(_, _, _, _, S1),
                     subst(_, _, _, _, S2)) :-
    freeze(S2, S1 > S2).

do_recursion(dec(G), C, G, C).
do_recursion(rec(G), _, G, nocs).

rec_command_info(not, _, not).
rec_command_info(rec, G, rec(C)) :- copy_term(G, C).
rec_command_info(dec, G, dec(C)) :- copy_term(G, C).

increase_counter(Count1) :-
    refactor_context(count, Count),
    succ(Count, Count1),
    nb_set_refactor_context(count, Count1).

fix_exception(error(Error, stream(_,  Line, Row, Pos)), File,
              error(Error, file(File, Line, Row, Pos))) :- !.
fix_exception(E, _, E).

do_genmcmd(GenMCmd, File, Level, M, CS, Command, In, Max) :-
    decreasing_recursion(CS, Command),
    catch(call(GenMCmd, Level, M, Command, In),
          E1,
          ( fix_exception(E1, File, E),
            print_message(error, E),
            fail
          )),
    increase_counter(Count1),
    ( nonvar(Max),
      Count1 >= Max
    ->!
    ; true
    ).

apply_commands_stream(FPTerm, GenMCmd, File, Level, M, CS, Max, In, Text1, Text) :-
    IPosText = 0-"",
    rec_command_info(FPTerm, GenMCmd, CI),
    ignore(forall(do_genmcmd(GenMCmd, File, Level, M, CS, Command, In, Max),
                  apply_commands_stream_each(FPTerm, File, CI, M, Max, Command,
                                             Text1, IPosText)
                 )),
    IPosText = Pos-Text6,
    sub_string(Text1, Pos, _, 0, TText),
    string_concat(Text6, TText, Text).

apply_commands_stream_each(FPTerm, File, CI, M, Max, Command, Text1, IPosText) :-
    ( apply_change(Text1, M, Command, FromToPText1),
      ( do_recursion(CI, Command, GenMCmd, CS)
      ->FromToPText1 = t(From, To, PasteText),
        get_out_pos(Text1, IPosText, From, Line, LPos),
        with_output_to(atom(LeftText),
                       ( forall(between(2, Line, _), nl),
                         line_pos(LPos)
                       )),
        atomics_to_string([LeftText, PasteText, "."], Text2),
        set_refactor_context(text, Text2),
        setup_call_cleanup(
            ( open_codes_stream(Text2, In)
              % seek(In, Pos, bof, _)
            ),
            apply_commands_stream(FPTerm, GenMCmd, File, term, M, CS, Max, In,
                                  Text2, Text3),
            close(In)),
        set_refactor_context(text, Text1),
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
    sum_list(NL, T).

will_occurs(Var, Sent, Pattern, Into, N) :-
    occurrences_of_var(Var, Sent, SN),
    occurrences_of_var(Var, Pattern, PN),
    occurrences_of_var(Var, Into, IN),
    N is SN-PN+IN.

gen_new_variable_names([], _, _, _, _, _, _, VNL, VNL).
gen_new_variable_names([Var|VarL], [Name1|NameL], Preffix, Count1,
                       Sent, Pattern, Into, VNL1, VNL) :-
    ( nonvar(Name1)
    ->VNL2 = VNL1,
      Count = Count1
    ; will_occurs(Var, Sent, Pattern, Into, VNL1, N),
      N > 1
    ->gen_new_variable_name(VNL1, Preffix, Count1, Name),
      succ(Count1, Count),
      VNL2 = [Name=Var|VNL1]
    ; VNL2 = ['_'=Var|VNL1],
      Count = Count1
    ),
    gen_new_variable_names(VarL, NameL, Preffix, Count, Sent, Pattern, Into, VNL2, VNL).

apply_change(Text, M, subst(TermPos, Options, Term, Into, _),
             t(From, To, PasteText)) :-
    call_cleanup(
        with_output_to(
            string(OutputText),
            ( stream_property(current_output, position(Pos1)),
              with_termpos(
                  print_expansion_1(Into, Term, TermPos,
                                    [module(M),
                                     text(Text)|Options],
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

call_expander(Expander, TermPos, Pattern, Into) :-
    refactor_context(tries, Tries),
    refactor_context(max_tries, MaxTries),
    ( nonvar(MaxTries)
    ->Tries < MaxTries
    ; true
    ),
    succ(Tries, Tries1),
    nb_set_refactor_context(tries, Tries1),
    with_refactor_context(catch(once(Expander), Error,
                              ( refactor_message(error, Error),
                                fail
                              )),
                        [termpos, pattern, into],
                        [TermPos, Pattern, Into]).

special_term(top, Term1, Into1, Into7, Into) :-
    ( nonvar(Term1),
      memberchk(Term1, [[], end_of_file])
    ->( \+ is_list(Into1)
      ->List = [Into7]
      ; List = Into7
      ),
      Into = '$LISTC'(List)
    ; var(Into1)
    ->Into = Into7
    ; is_list(Into1),
      same_length(Into1, Term1)
    ->Into = Into7
    ; Into1 = [_|_]
    ->Into = '$LISTC.NL'(Into7)
    ; Into1 = []
    ->Into = '$RM'
    ; Into = Into7
    ).
special_term(sub_cw, _, _, Term, Term).
special_term(sub,    _, _, Term, Term).

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

remove_hacks(H, T) :-
    trim_hacks(H, S),
    deref_substitution(S, T).

match_vars_with_names(VNL1, Var, Name) :-
    ignore(( member(Name=Var1, VNL1),
             Var == Var1
           )).

gen_new_variable_names(Sent, Term, Into, VNL, NewVNL) :-
    refactor_context(preffix, Preffix),
    refactor_context(variable_names, VNL1),
    trim_hacks(Into, TInto),
    term_variables(TInto, VarL),
    maplist(match_vars_with_names(VNL1), VarL, NameL),
    gen_new_variable_names(VarL, NameL, Preffix, 1, Sent, Term, TInto, VNL1, VNL),
    once(append(NewVNL, VNL1, VNL)).

check_bindings(Sent, Sent2, Options) :-
    ( Sent=@=Sent2
    ->true
    ; option(show_left_bindings(Show), Options, true),
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

%!  substitute_term_norec(+Sub, +M, +Term, +Priority, +Pattern, +Into, :Expander, +TermPos, SentPos, Cmd) is nondet.
%
%   Non-recursive version of substitute_term_rec/6.

substitute_term_norec(Sub, M, Term, TermPos1, Priority, data(Pattern1, Into1, Expander, SentPos),
                      subst(TermPos1,
                            SubstOptions,
                            Term, Into, Size)) :-
    wr_options(WriteOptions),
    refactor_context(sentence,     Sent),
    refactor_context(sent_pattern, SentPattern),
    subsumes_term(SentPattern-Pattern1, Sent-Term),
    refactor_context(options, Options),
    merge_options([priority(Priority),
                   variable_names(VNL),
                   new_variable_names(NewVNL)
                   |WriteOptions], Options, SubstOptions),
    option(decrease_metric(Metric), Options, ref_replace:pattern_size),
    call(Metric, Term, Pattern1, Size),
    with_context(Sub, M, Term, TermPos1, Priority, Sent, SentPos, Pattern1, Into1, Into, VNL, NewVNL, Expander, Options).

val_subs(V, S) -->
    ( {var(S)}
    ->[V=S]
    ; []
    ).

with_context(Sub, M, Term1, TermPos1, Priority, Sent1, SentPos1, Pattern1, Into1, Into, VNL, NewVNL, Expander1, Options) :-
    % Suffix numbers in variables should refer to:
    % 1: Term changes during Expander1 execution
    % 3: The raw Term, as read from the file
    % 4: Pattern changes during Expander1 execution
    % 5: Original pattern
    refactor_context(sent_pattern, SentPattern1),
    copy_term(SentPattern1-Pattern1-Into1, _Sent5-Term5-Into5),
    copy_term(SentPattern1-Pattern1-Into1, _Sent4-Term4-Into4),
    Pattern1 = Term1,
    SentPattern1 = Sent1,
    term_variables(Sent1-Term1-Into1, Vars1),
    copy_term(Sent1-Term1-Into1-Vars1, Sent3-Term3-Into3-Vars3),
    call_expander(Expander1, TermPos1, Term4, Into4),
    foldl(val_subs, Vars3, Vars1, ValSubs, []),
    substitute_values(ValSubs, Sent3-Term3, Sent2-Term2),
    check_bindings(Sent2, Sent3, Options),
    gen_new_variable_names(Sent1, Term1, Into1, VNL, NewVNL),
    trim_fake_pos(TermPos1, TTermPos1, N),
    substitute_value(TermPos1, TTermPos1, SentPos1, TSentPos1),
    trim_fake_args_ll(N, [[Term5, Term4, Term3, Term2], [Term2],
                          [Into5, Into4, Into3, Into1], [Into1]],
                      [TermL, [TTerm1], IntoL, [TInto1]]),
    /* Note: fix_subtermpos/1 is a very expensive predicate, due to that we
       delay its execution until its result be really needed, and we only
       apply it to the subterm positions being affected by the refactoring.
       The predicate performs destructive assignment (as in imperative
       languages), modifying term position once the predicate is called */
    fix_subtermpos(TTerm1, TInto1, Sub, TSentPos1, Options),
    replace_subterm_locations(NewVNL, TermL, TTerm1, IntoL, TInto1, M, TTermPos1, Priority, TInto7),
    special_term(Sub, TTerm1, TInto1, TInto7, Into).

sleq(Term, Into, Term) :- Term == Into.

subterm_location_same_term([], Term1, Term2, Term1) :-
    same_term(Term1, Term2),
    !.
subterm_location_same_term([N|L], Term1, Term2, SubTerm) :-
    compound(Term1),
    arg(N, Term1, SubTerm1),
    arg(N, Term2, SubTerm2),
    subterm_location_same_term(L, SubTerm1, SubTerm2, SubTerm).

:- thread_local partial_path_db/1.

is_scanneable(Term) :-
    compound(Term),
    \+ memberchk(Term, ['$@'(_)]).

find_term_path(Term2, Into2, [TermLoc2, IntoLoc2, ArgLoc2, SubLoc2], [TermLoc1, IntoLoc1, ArgLoc1, SubLoc1]) :-
    ( Into2 \== Term2,
      location_subterm_un(IntoLoc2, Into2, is_scanneable, Sub2),
      location_subterm_eq(TermLoc2, Term2, Sub2),
      ArgLoc1 = SubLoc1
    ; ArgLoc2 = [],
      SubLoc2 = []
    ),
    append(IntoLoc2, SubLoc1, IntoLoc1),
    append(TermLoc2, ArgLoc1, TermLoc1).

curr_subterm_replacement(TermL, Term1, IntoL, Into1, TermLoc1, IntoLoc1, ArgLocL, Size) :-
    retractall(partial_path_db(_)),
    foldl(find_term_path, TermL, IntoL, [TermLoc, IntoLoc, TermLoc, IntoLoc], [TermLoc1, IntoLoc1, _, _]),
    location_subterm_un(IntoLoc1, Into1, is_scanneable, Sub1),
    \+ partial_path_db(IntoLoc1),
    % Sub1 \== [],
    subterm_location(sleq(Arg1, Sub1), Term1, TermLoc1),
    append(IntoLoc1, _, PIntoLoc1),
    assertz(partial_path_db(PIntoLoc1)),
    findall([Ord1, ArgLoc],
            ( subterm_location_same_term(ArgLoc, Arg1, Sub1, ToRep),
              term_size(ToRep, Size1),
              Ord1 is -Size1
            ), ArgLocLU),
    sort(ArgLocLU, ArgLocLL),
    transpose(ArgLocLL, [[Ord1|_], ArgLocL]),
    Size is -Ord1.

replace_subterm_locations(VNL, TermL, Term1, IntoL, Into1, M, TermPos, Priority, Into) :-
    findall(([TermLoc1, IntoLoc1]-ArgLocL),
            order_by([desc(Size)],
                     curr_subterm_replacement(TermL, Term1, IntoL, Into1, TermLoc1, IntoLoc1, ArgLocL, Size)),
            TermLocArgLocLL),
    foldl(perform_replacement(VNL, M, TermPos, Priority, Term1, Into1), TermLocArgLocLL, Into1-[], Into-VL),
    maplist(collapse_bindings, VL).

collapse_bindings(A=B) :- ignore(A=B).

perform_replacement(VNL, M, TermPos, Priority1, Term1, Into1, [TermLoc, IntoLoc]-ArgLocL, TInto1-VL1, TInto-[Var1=Rep1|VL1]) :-
    % location_subterm_un(TermLoc, Term1, Sub1),
    location_subterm_un(IntoLoc, Into1, Arg1),
    subpos_location(TermLoc, TermPos, SubPos),
    foldl(perform_replacement_2(VNL, SubPos, Arg1), ArgLocL, RepU, []),
    sort(RepU, RepL),
    ( append(L1, [E], TermLoc),
      location_subterm_un(L1, Term1, TP),
      term_priority(TP, M, E, Priority)
    ->true
    ; Priority = Priority1
    ),
    compound(SubPos),
    arg(1, SubPos, From),
    arg(2, SubPos, To),
    From \= To,
    get_innerpos(SubPos, ISubPos),
    Rep1 = '$sb'(SubPos, ISubPos, RepL, Priority, Arg1),
    replace_at_subterm_location(IntoLoc, Var1, TInto1, TInto),
    !.
perform_replacement(_, _, _, _, _, _, _, IntoVL, IntoVL).

get_innerpos(OSubPos, ISubPos) :-
    OSubPos =.. [F, OFrom, OTo|Args],
    term_innerpos(OFrom, OTo, IFrom, ITo),
    !,
    ISubPos =.. [F, IFrom, ITo|Args].
get_innerpos(SubPos, SubPos).

replace_at_subterm_location([], Rep, _, Rep).
replace_at_subterm_location([N|L], Rep, Term1, Term2) :-
    compound(Term1),
    compound_name_arguments(Term1, Name, Args1),
    length([_|Left], N),
    append(Left, [Arg1|Right], Args1),
    append(Left, [Arg2|Right], Args2),
    compound_name_arguments(Term2, Name, Args2),
    replace_at_subterm_location(L, Rep, Arg1, Arg2).

perform_replacement_2(VNL, SubPos, Arg1, ArgLoc) -->
    { subpos_location(ArgLoc, SubPos, ArgPos),
      location_subterm_un(ArgLoc, Arg1, ToRep1)
    },
    ( {var(ToRep1)}
    ->( { member(Name = Var, VNL),
          ToRep1 == Var
        }
      ->['$sb'(ArgPos, '$VAR'(Name))]
      ; []
      )
    ; []
    ).

fake_pos(T-T).

%!  trim_fake_pos(+TermPos, -Pos, -N)
%
%   remove fake arguments that would be added by dcg
trim_fake_pos(Pos1, Pos, N) :-
    ( nonvar(Pos1),
      Pos1 = term_position(F, T, FF, FT, PosL1),
      nonvar(PosL1)
    ->partition(fake_pos, PosL1, FakePosL, PosL),
      length(FakePosL, N),
      Pos = term_position(F, T, FF, FT, PosL)
    ; Pos = Pos1,
      N = 0
    ).

trim_fake_args_ll(N) --> maplist(maplist(trim_fake_args(N))).

trim_fake_args(N, Term1, Term) :-
    ( N > 0,
      Term1 =.. ATerm1,
      length(TE, N),
      append(ATerm, TE, ATerm1),
      Term =.. ATerm
    ->true
    ; Term = Term1
    ).

%!  substitute_term_rec(+Module, +Term, +TermPos, +Priority, +Data, -Cmd) is nondet.
%
%   True when Cmd contains a substitution for Pattern by Into in
%   SrcTerm, where Data = data(Pattern, Into, Expander, SentPos).
%   This predicate must be cautious about handling bindings:
%
%   - Overall bindings do not affect further substitutions because they are
%     managed by findall/3 in do_replace/6.
%   - Pattern must not be instantiated by either unification with SrcTerm or the
%     execution of Expander.  This is needed for substitute_term/7 to find the
%     correct replacements.
%
%   To avoid binding Pattern, we need to copy Pattern and Into while maintaining
%   sharing with Expander.  Next, we can safely unify Pattern with the SrcTerm.

substitute_term_rec(M, Term, TermPos, Priority, Data, Cmd) :-
    substitute_term_norec(sub, M, Term, TermPos, Priority, Data, Cmd),
    !.
substitute_term_rec(M, Term, TermPos, _, Data, Cmd) :-
    substitute_term_into(TermPos, M, Term, Data, Cmd).

substitute_term_into(brace_term_position(_, _, Pos), M, {Term}, Data, Cmd) :-
    substitute_term_rec(M, Term, Pos, 1200, Data, Cmd).
substitute_term_into(parentheses_term_position(_, _, Pos), M, Term, Data, Cmd) :-
    substitute_term_rec(M, Term, Pos, 1200, Data, Cmd).
substitute_term_into(term_position(_, _, _, _, PosL), M, Term, Data, Cmd) :-
    substitute_term_args(PosL, M, Term, Data, Cmd).
substitute_term_into(list_position(_, _, EP, TP), M, Term, Data, Cmd) :-
    substitute_term_list(EP, TP, M, Term, Data, Cmd).
substitute_term_into(map_position(_, _, _, _, PosL), M, Term, Data, Cmd) :-
    member(Pos, PosL),
    substitute_term_pair(M, Term, Pos, Data, Cmd).

substitute_term_pair(M, Term, key_value_position(_, _, Key, PosK, PosV), Data, Cmd) :-
    ( substitute_term_rec(M, Key, PosK, 999, Data, Cmd)
    ; substitute_term_rec(M, Term.Key, PosV, 999, Data, Cmd)
    ).

:- use_module(library(listing), []).

term_priority(Term, M, N, Priority) :-
    compound(Term),
    term_priority_gnd(Term, M, N, PrG),
    ( arg(N, Term, Arg),
      term_needs_braces(M:Arg, PrG)
    ->Priority = 999
    ; Priority = PrG
    ).

term_priority_gnd(Term, M, N, PrG) :-
    functor(Term, F, A),
    ( ( A == 1
      ->( prolog_listing:prefix_op(M:F, PrG) -> true
        ; prolog_listing:postfix_op(M:F, PrG) -> true
        )
      ; A == 2
      ->prolog_listing:infix_op(M:F, Left, Right),
        ( N==1 -> PrG = Left
        ; N==2 -> PrG = Right
        )
      )
    ->true
    ; term_priority((_, _), user, 1, PrG)
    ).

substitute_term_args(PAL, M, Term, Data, Cmd) :-
    nth1(N, PAL, PA),
    arg(N, Term, Arg),
    term_priority(Term, M, N, Priority),
    substitute_term_rec(M, Arg, PA, Priority, Data, Cmd).

substitute_term_list([EP|EPs], TP, M, [Elem|Term], Data, Cmd) :-
    ( term_priority([_|_], M, 1, Priority),
      substitute_term_rec(M, Elem, EP, Priority, Data, Cmd)
    ; substitute_term_list(EPs, TP, M, Term, Data, Cmd)
    ).
substitute_term_list([], TP, M, Tail, Data, Cmd) :-
    term_priority([_|_], M, 2, Priority),
    substitute_term_rec(M, Tail, TP, Priority, Data, Cmd).

compound_positions(Line1, Pos2, Pos1, Pos) :- Line1 =< 1, !, Pos is Pos1+Pos2.
compound_positions(_, Pos, _, Pos).

get_output_position(Pos) :-
    ( refactor_context(from, From)
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

rportray_clause(Clause, OptL) :- rportray_clause(Clause, 0, OptL).

% We can not use portray_clause/3 because it does not handle the hooks
% portray_clause_(OptL, Clause) :-
%     portray_clause(current_output, Clause, OptL).

rportray_clause(C, Pos, OptL1) :-
    option(module(M), OptL1),
    stream_property(current_output, position(SPos1)),
    merge_options([portray_clause(false), partial(false)], OptL1, OptL2),
    write(''),
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
      ->write(''),
        write_term(H, OptL3),
        functor(C, Neck, _),
        write(' '),
        writeln(Neck),
        line_pos(4+BPos),
        term_priority((_, _), M, 2, Priority),
        merge_options([priority(Priority)], OptL3, OptL4),
        write_b(B, OptL4, 4+BPos)
      ; write(''),
        write_term(C, OptL3)
      ),
      cond_display(Display, ')')
    ; true
    ).

deref_substitution(Var, Var) :- var(Var), !.
deref_substitution('$sb'(_, _, _, _, Term), Sub) :-
    !,
    deref_substitution(Term, Sub).
deref_substitution(Term, Term).

write_pos_lines(Pos, Writter, Lines) :-
    write_pos_rawstr(Pos, Writter, String),
    atomics_to_string(Lines, '\n', String).

write_pos_rawstr(Pos, Writter, String) :-
    with_output_to(string(String1),
                   ( nl, % start with a new line, since the position is not reseted
                     stream_property(current_output, position(Pos1)),
                     line_pos(Pos),
                     call(Writter),
                     stream_property(current_output, position(Pos2)),
                     stream_position_data(char_count, Pos1, B1),
                     stream_position_data(char_count, Pos2, B2)
                   )),
    L is B2-B1,
    sub_string(String1, B1, L, _, String).

write_pos_string(Pos, Writter, String) :-
    write_pos_rawstr(Pos, Writter, RawStr),
    pos_indent(Pos, Indent),
    atom_concat(Indent, String, RawStr).

write_term_lines(Pos, Opt, Term, Lines) :-
    write_pos_lines(Pos, write_term(Term, Opt), Lines).

write_term_string(Pos, Opt, Term, String) :-
    write_pos_string(Pos, write_term(Term, Opt), String).

print_subtext_sb_1(Text, Options, '$sb'(SubPos, Term), From, To) :-
    arg(1, SubPos, SubFrom),
    print_subtext(From-SubFrom, Text),
    write_term(Term, Options),
    arg(2, SubPos, To).

print_subtext_sb_2(Term, TermPos, RepL, Priority, Text, Options) :-
    with_cond_braces_2(print_subtext_2, Term, TermPos, RepL, Priority, Text, Options).

with_cond_braces_2(Call, Term, TermPos, RepL, GPriority, Text, Options) :-
    option(module(M), Options),
    option(priority(Priority), Options),
    fix_position_if_braced(TermPos, M, Term, GPriority, Term, Priority, Display),
    cond_display(Display, '('),
    call(Call, TermPos, RepL, Text, Options),
    cond_display(Display, ')').

print_subtext_2(sub_list_position(BFrom, To, BTo, From, PosL, Tail), RepL, Text, Options) :-
    !,
    print_subtext(BFrom-BTo, Text),
    print_subtext_2(list_position(From, To, PosL, Tail), RepL, Text, Options).
print_subtext_2(TermPos, RepL, Text, Options) :-
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    foldl(print_subtext_sb_1(Text, Options), RepL, From, SubTo),
    print_subtext(SubTo-To, Text).

:- public
    rportray/2.

/*
rportray('$sb'(TermPos), _) :-
    \+ retract(rportray_skip),
    !,
    refactor_context(text, Text),
    print_subtext(TermPos, Text).
*/
rportray('$sb'(SubPos, _, RepL, Priority, Term), Options) :-
    \+ retract(rportray_skip),
    !,
    ignore(( option(text(Text), Options),
             print_subtext_sb_2(Term, SubPos, RepL, Priority, Text, Options)
           )).
rportray('$@'(Term), Options) :- write_term(Term, Options).
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
      STerm = '$sb'(OTermPos, ITermPos, _, _, _)
    ->arg(1, ITermPos, IFrom),
      arg(2, ITermPos, ITo),
      arg(1, OTermPos, OFrom),
      arg(2, OTermPos, OTo),
      option(text(Text), Options),
      print_subtext(OFrom-IFrom, Text),
      write_term(Term, Options),
      print_subtext(ITo-OTo, Text)
    ; write_term(Term, Options)
    ).
% Use a different pattern to guide the printing of Term:
rportray('$@'(Into, '$sb'(_, SubPos, _, Priority, Term)), Options) :-
    !,
    option(text(Text), Options),
    once(print_expansion_sb(Into, Term, SubPos, Priority, Options, Text)).
rportray('$G'(Into, Goal), Opt) :-
    callable(Goal),
    \+ special_term(Goal),
    !,
    with_str_hook(write_term(Into, Opt), Goal).
rportray('$C'(Goal, Into), Opt) :-
    callable(Goal),
    \+ special_term(Goal),
    !,
    call(Goal),
    write_term(Into, Opt).
% Ignore, but process for the side effects
rportray('$NOOP', _) :- !.
rportray('$NOOP'(Term), Opt) :-
    !,
    with_output_to(string(_), write_term(Term, Opt)).
rportray('$TEXT'(T), Opt) :- !, write_t(T, Opt).
rportray('$TEXT'(T, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    line_pos(Pos),
    write_t(T, Opt).
rportray('$TEXTQ'(T), Opt) :- !, write_q(T, Opt).
rportray('$TEXTQ'(T, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    line_pos(Pos),
    write_q(T, Opt).
rportray(H :- B, Opt) :-
    option(portray_clause(true), Opt),
    !,
    offset_pos('$OUTPOS', Pos),
    rportray_clause((H :- B), Pos, Opt).
rportray(H --> B, Opt) :-
    option(portray_clause(true), Opt),
    !,
    offset_pos('$OUTPOS', Pos),
    rportray_clause((H --> B), Pos, Opt).
rportray('$CLAUSE'(C), Opt) :- !, rportray_clause(C, Opt).
rportray('$CLAUSE'(C, Offs), Opt) :-
    !,
    offset_pos(Offs, Pos),
    rportray_clause(C, Pos, Opt).
rportray('$BODY'(B, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    rportray_body(B, Pos, Opt).
rportray('$BODY'(B), Opt) :-
    !,
    offset_pos('$OUTPOS', Pos),
    rportray_body(B, Pos, Opt).
rportray('$BODYB'(B, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    rportray_bodyb(B, Pos, Opt).
rportray('$BODYB'(B), Opt) :-
    !,
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
rportray('$APP'(L1, L2), Opt) :-
    !,
    ( nonvar(L1),
      L1 = '$sb'(OTermPos, ITermPos, RepL1, Priority, Term)
    ->once(( ITermPos = list_position(_, LTo, _, Pos)
           ; ITermPos = sub_list_position(_, LTo, _, _, _, Pos)
           ; Pos = ITermPos
           )),
      ( Pos = none
      ->succ(From, LTo),
        ( trim_brackets(L2, L3, Opt)
        ->remove_hacks(L3, T3),
          ( T3 == []
          ->sort(['$sb'(From-From, L3)|RepL1], RepL)
          ; sort(['$sb'(From-From, '$,'('$TEXT'(', '), L3))|RepL1], RepL)
          )
        ; sort(['$sb'(From-From, '$,'('$TEXT'('|'), L2))|RepL1], RepL)
        )
      ; arg(1, Pos, From),
        arg(2, Pos, To),
        sort(['$sb'(From-To, L2)|RepL1], RepL)
      ),
      write_term('$sb'(OTermPos, ITermPos, RepL, Priority, Term), Opt)
    ; append(L, T, L1),
      ( var(T)
      ; T \= [_|_]
      )
    ->append(L, L2, N),
      write_term(N, Opt)
    ).
rportray('$,'(A, B), Opt) :- !, write_term(A, Opt), write_term(B, Opt).
rportray('$LIST'(L), Opt) :- !, rportray_list(L, write_term, '', Opt).
rportray('$LIST,'(L), Opt) :- !, rportray_list(L, write_term, ',', Opt).
rportray('$LIST,_'(L), Opt) :- !, maplist(term_write_comma_2(Opt), L).
rportray('$LIST'(L, Sep), Opt) :- !, rportray_list(L, write_term, Sep, Opt).
rportray('$LISTC'(CL), Opt) :-
    !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(CL, rportray_clause_dot_nl, '', Opt1).
rportray('$LISTC.NL'(CL), Opt) :-
    !,
    merge_options([priority(1200), portray_clause(true)], Opt, Opt1),
    option(text(Text), Opt),
    term_write_sep_list_2(CL, rportray_clause, Text, '.\n', '.\n', Opt1).
rportray('$LIST.NL'(L), Opt) :-
    !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(L, write_term_dot_nl, '', Opt1).
rportray('$LISTNL.'(L), Opt) :-
    !,
    merge_options([priority(1200)], Opt, Opt1),
    rportray_list(L, write_term, '.\n', Opt1).
rportray('$LIST,NL'(L), Opt) :-
    offset_pos('$OUTPOS', Pos),
    !,
    rportray_list_nl_comma(L, Pos, Opt).
rportray('$LISTNL'(L), Opt) :-
    offset_pos('$OUTPOS', Pos),
    !,
    rportray_list_nl(L, Pos, Opt).
rportray('$TAB'(Term, Offs), Opt) :-
    offset_pos(Offs-'$OUTPOS', Delta),
    !,
    forall(between(1, Delta, _), write(' ')),
    write_term(Term, Opt).
rportray('$LIST,NL'(L, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    rportray_list_nl_comma(L, Pos, Opt).
rportray('$LISTNL'(L, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    rportray_list_nl(L, Pos, Opt).
rportray('$LISTB,NL'(L), Opt) :-
    offset_pos('$OUTPOS'+1, Pos),
    !,
    deref_substitution(L, D),
    rportray_list_nl_b(D, Pos, Opt).
rportray('$LISTB,NL'(L, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
    deref_substitution(L, D),
    rportray_list_nl_b(D, Pos, Opt).
rportray('$NL'(Term, Offs), Opt) :-
    offset_pos(Offs, Pos),
    !,
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
    integer(Priority),
    !,
    merge_options([priority(Priority)], Opt, Opt1),
    write_term(T, Opt1).
rportray(\+ Term, Opt) :-
    !,
    write_t('\\+ ', Opt),
    write(''),
    term_priority((_, _), user, 1, Priority),
    merge_options([priority(Priority)], Opt, Opt1),
    write_term(Term, Opt1).
rportray((A, B), Opt) :-
    !,
    sequence_list((A, B), L, []),
    once(append(T, [E], L)),
    offset_pos('$OUTPOS', Pos),
    term_priority((_, _), user, 1, Priority),
    option(priority(Pri), Opt),
    ( Priority > Pri
    ->Display = yes
    ; Display = no
    ),
    merge_options([priority(Priority)], Opt, Opt1),
    term_priority((_, _), user, 2, RPri),
    merge_options([priority(RPri)], Opt, Opt2),
    ( ( Display = yes
      ->Format ="(~s~s)",
        succ(Pos, Pos1)
      ; Format = "~s~s",
        Pos1 = Pos
      ),
      length(L, Length),
      pos_indent(Pos1, Indent),
      maplist([Pos1, Opt1, Indent] +\ E^Line^( write_term_lines(Pos1, Opt1, E, Lines),
                                              Lines = [Line1],
                                              string_concat(Indent, Line, Line1)
                                            ), T, LineL1),
      write_term_lines(Pos1, Opt2, E, LastLines1),
      LastLines1 = [LastLine1],
      atom_concat(Indent, LastLine, LastLine1),
      append(LineL1, [LastLine], StringL),
      maplist(string_length, StringL, WidthL),
      sum_list(WidthL, WidthTotal),
      Sep = ", ",
      string_length(Sep, SepLength),
      option(conj_width(ConjWidth), Opt),
      Pos1 + WidthTotal + (Length - 1) * SepLength < ConjWidth
    ->CloseB = ""
    ; ( Display = yes
      ->Format = "( ~s~s)",
        Pos1 = Pos + 2,
        with_output_to(string(CloseB),
                       ( nl,
                         line_pos(Pos)
                       ))
      ; Format = "~s~s",
        CloseB = "",
        Pos1 = Pos
      ),
      maplist(write_term_string(Pos1, Opt1), T, StringL1),
      write_term_string(Pos1, Opt2, E, LastStr),
      append(StringL1, [LastStr], StringL),
      sep_nl(Pos1, ',', Sep)
    ),
    atomics_to_string(StringL, Sep, S),
    format(atom(Atom), Format, [S, CloseB]),
    write_t(Atom, Opt1).
rportray([E|T1], Opt) :-
    !,
    offset_pos('$OUTPOS', Pos),
    succ(Pos, Pos1),
    H = [_|_],
    append(H, T2, [E|T1]),
    ( nonvar(T2),
      T2 = '$sb'(OTermPos, ITermPos, _, _, Term),
      is_list(Term),
      compound(OTermPos),
      !,
      arg(1, OTermPos, TFrom),
      arg(2, OTermPos, TTo),
      arg(1, ITermPos, From),
      arg(2, ITermPos, To),
      write_term_string(Pos, Opt, T2, SB),
      sub_string(SB, 1, _, 1, SC),
      option(text(Text), Opt),
      get_subtext(Text, TFrom, From, SL),
      get_subtext(Text, To, TTo, SR),
      format(atom(ST), "~s~s~s", [SL, SC, SR]),
      ( Term == []
      ->T = H,
        EndText = ST
      ; append(H, ['$TEXT'(ST)], T),
        EndText = ""
      )
    ; T2 == [],
      T = H,
      EndText = ""
    ; once(( var(T2)
           ; T2 \= [_|_]
           )),
      T = H,
      write_term_string(Pos1, Opt, T2, ST),
      atom_concat('|', ST, EndText)
    ),
    !,
    write_t('[', Opt),
    term_priority([_|_], user, 1, Priority),
    merge_options([priority(Priority)], Opt, Opt1),
    maplist(write_term_lines(Pos1, Opt1), T, LinesL),
    pos_indent(Pos1, Indent),
    ( maplist([Indent] +\ [Line1]^Line^string_concat(Indent, Line, Line1), LinesL, StringL),
      Sep = ", ",
      string_length(Sep, SepLength),
      option(list_width(ListWidth), Opt),
      length(StringL, Length),
      maplist(string_length, StringL, WidthL),
      sum_list(WidthL, WidthTotal),
      Pos1 + WidthTotal + (Length - 1) * SepLength =< ListWidth,
      CloseB = "]"
    ->atomics_to_string(StringL, Sep, S)
    ; Sep = ",\n",
      with_output_to(string(CloseB),
                     ( nl,
                       line_pos(Pos),
                       write(']')
                     )),
      maplist(\ L^S^atomics_to_string(L, '\n', S), LinesL, StringL),
      atomics_to_string(StringL, Sep, S1),
      string_concat(Indent, S, S1)
    ),
    atomic_list_concat([S, EndText, CloseB], Atom),
    write_t(Atom, Opt1).
% Better formatting:
rportray((:- Decl), Opt) :-
    !,
    offset_pos('$OUTPOS', Pos),
    write(':- '),
    merge_options([priority(1200)], Opt, Opt1),
    option(module(M), Opt),
    ( Decl =.. [Name, Arg],
      once(( current_op(OptPri, Type, M:Name),
             valid_op_type_arity(Type, 1)
           )),
      option(priority(Pri), Opt),
      OptPri =< Pri
    ->NDecl =.. [Name, '$NL'('$BODY'(Arg), Pos+4)]
    ; NDecl = Decl
    ),
    write_term(NDecl, Opt1).
rportray(Operator, Opt) :-
    % Fix to avoid useless operator parenthesis
    atom(Operator),
    option(module(M), Opt),
    option(priority(Priority), Opt),
    current_op(OpPriority, _, M:Operator),
    OpPriority < Priority,
    !,
    write_q(Operator, Opt).
% Better formatting:
rportray(Term, OptL) :-
    callable(Term),
    \+ escape_term(Term),
    \+ ctrl(Term),
    \+ skip_format(Term),
    option(module(M), OptL),
    ( ( compact_format(Term)
      ; term_arithexpression(Term, M)
      )
    ->Space = ''
    ; Space = ' '
    ),
    option(term_width(TermWidth), OptL),
    ( Term =.. [Name, Left, Right],
      current_op(OptPri, Type, M:Name),
      valid_op_type_arity(Type, 2)
    ->option(priority(Pri), OptL),
      ( OptPri > Pri
      ->Display = yes
      ; Display = no
      ),
      term_priority_gnd(Term, M, 1, LP),
      merge_options([priority(LP)], OptL, OptL1),
      cond_display(Display, '('),
      offset_pos('$OUTPOS', Pos),
      write_term(Left, OptL1),
      write(Space),
      offset_pos('$OUTPOS', Pos2),
      term_priority_gnd(Term, M, 2, RP),
      merge_options([priority(RP)], OptL, OptL2),
      write_pos_lines(Pos2,
                      ( write_q(Name, Opt2),
                        write_t(Space, Opt2),
                        write(''),
                        write_term(Right, OptL2)
                      ), Lines),
      ( Lines = [Line],
        atom_length(Line, Width),
        Width =< TermWidth
      ->pos_indent(Pos2, Indent),
        atom_concat(Indent, Atom, Line),
        write_t(Atom, OptL2)
      ; write_pos_lines(Pos,
                        ( write_q(Name, Opt2),
                          write_t(Space, Opt2),
                          write(''),
                          write_term(Right, OptL2)
                        ), Lines2),
        ( ( maplist(string_length, Lines, WidthL),
            max_list(WidthL, Width),
            Width > TermWidth
          ; length(Lines2, Height2),
            length(Lines,  Height),
            Height2 < Height
          )
        ->nl,
          atomic_list_concat(Lines2, '\n', Atom)
        ; Lines = [Line1|Tail],
          pos_indent(Pos2, Indent),
          atom_concat(Indent, Line, Line1),
          atomic_list_concat([Line|Tail], '\n', Atom)
        ),
        write_t(Atom, OptL2)
      ),
      cond_display(Display, ')')
    ; \+ atomic(Term),
      Term =.. [Name|Args],
      Args = [_, _|_]
      % There is no need to move the argument to another line if the arity is 1,
      % however that could change in the future if we change the format
      % \+ ( Args = [_],
      %      current_op(_, Type, M:Name),
      %      valid_op_type_arity(Type, 1)
      %    )
    ->atom_length(Name, NL),
      offset_pos('$OUTPOS'+NL+1, Pos),
      merge_options([priority(999)], OptL, Opt1),
      maplist(write_term_lines(Pos, Opt1), Args, LinesL),
      pos_indent(Pos, Indent),
      foldl(collect_args(Indent, TermWidth), LinesL, (Pos-2)-[_|T], _-[]),
      atomic_list_concat(T, Atom),
      write_q(Name, Opt1),
      write(''),
      write_t('(',  Opt1),
      write_t(Atom, Opt1),
      write_t(')',  Opt1)
    ),
    !.

trim_brackets(L, _, _) :- var(L), !, fail.
trim_brackets(Term, Trim, Opt) :-
    member(Term-Trim, ['$@'(L, E)-'$@'(T, E),
                       '@@'(L, E)-'@@'(T, E)
                      ]),
    neck,
    trim_brackets(L, T, Opt).
trim_brackets('$sb'(OTermPos, ITermPos, RepL1, Priority, Term),
              '$sb'(OTermPos, ITermPos, RepL,  Priority, Term), _) :-
    once(( ITermPos = list_position(From, To, _, _)
         ; ITermPos = sub_list_position(From, To, _, _, _, _)
         ; ITermPos = From-To,
           Term == []
         )),
    succ(From, From1),
    succ(To1, To),
    sort(['$sb'(From-From1, '$NOOP'),
          '$sb'(To1-To, '$NOOP')
          |RepL1], RepL).
trim_brackets(L, '$TEXT'(S), Opt) :-
    L = [_|_],
    with_output_to(string(S1), write_term(L, Opt)),
    sub_string(S1, 1, _, 1, S).

pos_indent(Pos, Indent) :- with_output_to(atom(Indent), line_pos(Pos)).

collect_args(Indent, TermWidth, LineL, Pos1-[Sep, String|T], Pos-T) :-
    ( LineL = [Line1],
      string_concat(Indent, String, Line1),
      string_length(String, Width),
      Pos is Pos1 + 2 + Width,
      Pos < TermWidth
    ->Sep = ", "
    ; atom_concat(",\n", Indent, Sep),
      last(LineL, Last),
      string_length(Last, Pos),
      atomics_to_string(LineL, '\n', String1),
      string_concat(Indent, String, String1)
    ).

pos_value(Pos, Value) :-
    ( rportray_pos(Pos, Value)
    ->true
    ; Pos == '$OUTPOS'
    ->get_output_position(Value)
    ; fail
    ).

term_arithexpression(X, M) :-
    substitute(sanitize_hacks, X, Y),
    compat_arithexpression(Y, M).

sanitize_hacks(Term, Into) :-
    nonvar(Term),
    memberchk(Term, ['$sb'(_, _), '$sb'(_, _, _, _, Into)]).

compat_arithexpression(X, _) :- var(X), !.
compat_arithexpression(X, _) :- number(X), !.
compat_arithexpression(X, M) :- arithmetic:evaluable(X, M), !.
compat_arithexpression(X, M) :-
    callable(X),
    current_arithmetic_function(X),
    forall((compound(X), arg(_, X, V)), compat_arithexpression(V, M)).

arithexpression(X) :- number(X), !.
arithexpression(X) :-
    callable(X),
    current_arithmetic_function(X),
    forall((compound(X), arg(_, X, V)), arithexpression(V)).

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
    rportray_list_nl(',', L, Pos, Opt).

rportray_list_nl(L, Pos, Opt) :-
    rportray_list_nl('', L, Pos, Opt).

rportray_list_nl(Pre, L, Pos, Opt) :-
    term_priority([_|_], user, 1, Priority),
    merge_options([priority(Priority)], Opt, Opt1),
    sep_nl(Pos, Pre, Sep),
    rportray_list(L, write_term, Sep, Opt1).

rportray_list(L, Writter, SepElem, Opt) :-
    option(text(Text), Opt),
    deref_substitution(L, D),
    ( D = []
    ->true
    ; term_write_sep_list_2(D, Writter, Text, SepElem, '|', Opt)
    ).

term_write_sep_list_2([E|T], Writter, Text, SepElem, SepTail, Opt) :-
    !,
    call(Writter, E, Opt),
    term_write_sep_list_inner(T, Writter, Text, SepElem, SepTail, Opt).
term_write_sep_list_2(E, Writter, _, _, _, Opt) :- call(Writter, E, Opt).

term_write_sep_list_inner(L, Writter, Text, SepElem, SepTail, Opt) :-
    nonvar(L),
    L = [E|T],
    !,
    write(SepElem),
    call(Writter, E, Opt),
    term_write_sep_list_inner(T, Writter, Text, SepElem, SepTail, Opt).
term_write_sep_list_inner(T, Writter, Text, SepElem, SepTail, Opt) :-
    ( T == []
    ->true
    ; write_tail(T, Writter, Text, SepElem, SepTail, Opt)
    ).

term_write_comma_2(Opt, Term) :- write_term(Term, Opt), write(', ').

sep_nl(LinePos, Sep, SepNl) :-
    with_output_to(atom(In), line_pos(LinePos)),
    atomic_list_concat([Sep, '\n', In], SepNl).

write_tail(T, Writter, _, _, SepTail, Opt) :-
    var(T),
    !,
    write(SepTail),
    call(Writter, T, Opt).
write_tail([], _, _, _, _, _) :- !.
write_tail('$LIST,NL'(L), Writter, Text, _, _, Opt) :-
    !,
    offset_pos('$OUTPOS', Pos),
    sep_nl(Pos, ',', Sep),
    term_write_sep_list_inner(L, Writter, Text, Sep, '|', Opt).
write_tail('$LIST,NL'(L, Offs), Writter, Text, _, _, Opt) :-
    offset_pos(Offs, Pos),
    !,
    sep_nl(Pos, ',', Sep),
    term_write_sep_list_inner(L, Writter, Text, Sep, '|', Opt).
write_tail(T, Writter, _, _, SepTail, Opt) :-
    write(SepTail),
    call(Writter, T, Opt).

print_expansion_rm_dot(TermPos, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, Before),
    sub_string(Text, Before, _, 0, Right),
    sub_string(Right, Next, _, _, "."),
    To is Before+Next+2.

% Hacks that can only work at 1st level:

print_expansion_1(Into, Term, TermPos, Options, Text, From, To) :-
    get_innerpos(TermPos, ITermPos),
    print_expansion_2(Into, Term, ITermPos, Options, Text, From, To).

print_expansion_2(Into, Term, TermPos, Options, Text, From, To) :-
    var(Into),
    !,
    print_expansion_3(Into, Term, TermPos, Options, Text, From, To).
print_expansion_2('$RM', _, TermPos, _, Text, From, To) :-
    !,
    print_expansion_rm_dot(TermPos, Text, From, To).
print_expansion_2('$NODOT'(Into), Term, TermPos, Options, Text, From, To) :-
    !,
    print_expansion_2(Into, Term, TermPos, Options, Text, From, _To),
    print_expansion_rm_dot(TermPos, Text, _From, To).
print_expansion_2('$TEXT'(Into), _, TermPos, Options, _, From, To) :-
    !,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    write_t(Into, Options).
print_expansion_2('$TEXT'(Into, Offs), _, TermPos, Options, _, From, To) :-
    offset_pos(Offs, Pos),
    !,
    arg(1, TermPos, From),
    arg(2, TermPos, To1),
    write_t(Into, Options),
    To is To1+Pos.
print_expansion_2('$TEXTQ'(Into), _, TermPos, Options, _, From, To) :-
    !,
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    write_q(Into, Options).
print_expansion_2('$TEXTQ'(Into, Offs), _, TermPos, Options, _, From, To) :-
    offset_pos(Offs, Pos),
    !,
    arg(1, TermPos, From),
    arg(2, TermPos, To1),
    write_q(Into, Options),
    To is To1+Pos.
print_expansion_2('$LIST.NL'(IntoL), Term, TermPos, Options1, Text, From, To) :-
    !,
    merge_options([priority(1200)], Options1, Options),
    print_expansion_rm_dot(TermPos, Text, From, To),
    with_from(term_write_stop_nl_list(IntoL, Term, TermPos, Options, Text), From).
print_expansion_2(Into, Term, TermPos, Options, Text, From, To) :-
    print_expansion_3(Into, Term, TermPos, Options, Text, From, To).

term_write_stop_nl_list([Into|IntoL], Term, TermPos, Options, Text) :-
    term_write_stop_nl__(Into, Term, TermPos, Options, Text),
    term_write_stop_nl_list(IntoL, Term, TermPos, Options, Text).
term_write_stop_nl_list('$sb'(_, _, _, _, IntoL), Term, TermPos, Options, Text) :-
    term_write_stop_nl_list(IntoL, Term, TermPos, Options, Text).
term_write_stop_nl_list([], _, _, _, _).

term_write_stop_nl__('$NOOP'(Into), Term, TermPos, Options, Text) :- !,
    with_output_to(string(_),   %Ignore, but process
                   term_write_stop_nl__(Into, Term, TermPos, Options, Text)).
term_write_stop_nl__('$NODOT'(Into), Term, TermPos, Options, Text) :- !,
    print_expansion(Into, Term, TermPos, Options, Text).
term_write_stop_nl__(Into, Term, TermPos, Options, Text) :-
    print_expansion(Into, Term, TermPos, Options, Text),
    write('.'),
    nl.

print_expansion_3(Into, Term, TermPos, Options, Text, From, To) :-
    arg(1, TermPos, From),
    arg(2, TermPos, To),
    with_from(print_expansion(Into, Term, TermPos, Options, Text), From).

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
    ->Display = yes
    ),
    !.
fix_position_if_braced(_, _, _, _, _, _, no). % fail-safe

% If Term is a replacement, '$sb'/6, we assume that the substitution will not
% require braces (not sure if this is correct, but it works)
term_needs_braces(_:Term, _) :- \+ callable(Term), !, fail.
% term_needs_braces(M:'$sb'(_, _, _, _, _, Into), Pri) :- !,
%     term_needs_braces(M:Into, Pri).
term_needs_braces(M:Term, Pri) :- term_needs_braces(Term, M, Pri).

term_needs_braces(Term, M, Pri) :-
    functor(Term, Name, Arity),
    valid_op_type_arity(Type, Arity),
    current_op(OpPri, Type, M:Name),
    OpPri > Pri,
    !.

cond_display(yes, A) :- write(A).
cond_display(no, _).

:- meta_predicate
    with_cond_braces(5, +, +, +, +, +, +).

print_expansion_sb(Into, Term, TermPos, Priority, Options, Text) :-
    with_cond_braces(do_print_expansion_sb, Into, Term, TermPos, Priority, Options, Text).

do_print_expansion_sb(Into, Term, TermPos, Options, Text) :-
    arg(1, TermPos, From),
    with_from(print_expansion_ne(Into, Term, TermPos, Options, Text), From).

with_cond_braces(Call, Into, Term, TermPos, GPriority, Options, Text) :-
    option(module(M), Options),
    option(priority(Priority), Options),
    fix_position_if_braced(TermPos, M, Term, GPriority, Into, Priority, Display),
    cond_display(Display, '('),
    call(Call, Into, Term, TermPos, Options, Text),
    cond_display(Display, ')').

% TODO: stream position would be biased --EMM
with_str_hook(Command, StrHook) :-
    with_output_to(string(S1), call(Command)),
    ( call(StrHook, S1, S)
    ->true
    ; S = S1
    ),
    format('~s', [S]).

%!  print_expansion(?Into:term, ?Term:Term, RefPos, Priority:integer, Options:list, Text:string) is det
%
print_expansion(Var, _, RefPos, Options, Text) :-
    var(Var),
    !,
    option(new_variable_names(VNL), Options, []),
    ( member(Name=Var1, VNL),
      Var1 == Var
    ->write(Name)
    ; print_subtext(RefPos, Text)
    ).
print_expansion('$sb'(RefPos, _), Term, _, _, Text) :-
    \+ ( nonvar(Term),
         Term = '$sb'(_, _)
       ),
    !,
    print_subtext(RefPos, Text).
print_expansion('$sb'(RefPos, _, RepL, Priority, Into), Term, _RPos, Options, Text) :-
    nonvar(RefPos),
    \+ ( nonvar(Term),
         Term = '$sb'(_, _, _, _, _),
         Into \= '$sb'(_, _, _, _, _)
       ),
    !,
    print_subtext_sb_2(Into, RefPos, RepL, Priority, Text, Options).
print_expansion(Into, Term, RefPos, Options, Text) :-
    print_expansion_ne(Into, Term, RefPos, Options, Text).

print_expansion_ne('$G'(Into, Goal), Term, RefPos, Options, Text) :-
    \+ ( nonvar(Term),
         Term = '$G'(_, _)
       ),
    !,
    with_str_hook(print_expansion(Into, Term, RefPos, Options, Text), Goal).
print_expansion_ne('$C'(Goal, Into), Term, RefPos, Options, Text) :-
    \+ ( nonvar(Term),
         Term = '$C'(_, _)
       ),
    !,
    call(Goal),
    print_expansion(Into, Term, RefPos, Options, Text).
print_expansion_ne('$,NL', Term, RefPos, Options, Text) :-
    Term \=='$,NL',
    !,
    %% Print a comma + indented new line
    write(','),
    print_expansion('$NL', Term, RefPos, Options, Text).
print_expansion_ne('$NL', Term, _, _, Text) :- % Print an indented new line
    Term \== '$NL',
    !,
    refactor_context(from, From),
    textpos_line(Text, From, _, LinePos),
    nl,
    line_pos(LinePos).
/*
print_expansion_ne(Into, Term1, _, Options, Text) :-
    nonvar(Term1),
    Term1\='$sb'(_, _, _, _), % is not a read term, but a command
    SPattern='$sb'(RefPos, _, _, Term, Pattern),
    !,
    print_expansion_ne(Into, Pattern, Term, RefPos, Options, Text).
*/
print_expansion_ne(Into, Term, RefPos, Options, Text) :-
    ( \+ escape_term(Into),
      print_expansion_pos(RefPos, Into, Term, Options, Text)
    ->true
    ; write_term(Into, Options)
    ).

print_expansion_arg(M, MInto, Options1, Text, Length, I, From-To,
                    v(N, RefPos, Into, Term), Freeze1, Freeze) :-
    ( N = 0,
      Into == Term
    ->Freeze1 = true,
      print_subtext(RefPos, Text),
      freeze(Freeze, print_subtext(Text, From, To))
    ; term_priority(MInto, M, N, Priority),
      merge_options([priority(Priority)], Options1, Options),
      print_expansion_elem(Options, Text, Length, I, From-To, RefPos, Into, Term, Freeze1, Freeze)
    ).

print_expansion_elem(Options, Text, Length, I, From-To, RefPos, Into, Term, Freeze1, Freeze) :-
    ( Into == '$RM',
      Term \== '$RM'
    ->( Length = I
      ->Freeze = true
      ; Freeze = Freeze1
      )
    ; Freeze1 = true,
      print_expansion(Into, Term, RefPos, Options, Text),
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

special_term('$sb'(_, _)).
special_term('$sb'(_, _, _, _, _)).

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

print_expansion_pos(term_position(From, To, FFrom, FFTo, PosT), Into, Term, Options, Text) :-
    compound(Into),
    Into \= [_|_],
    nonvar(Term),
    functor(Into, FT, A),
    functor(Term, FP, A),
    option(module(M), Options),
    ( option(priority(Priority), Options),
      current_op(PrP, TypeOpP, M:FP),
      valid_op_type_arity(TypeOpP, A),
      current_op(PrT, TypeOpT, M:FT),
      valid_op_type_arity(TypeOpT, A),
      PrT =< Priority,
      ( PrP =< PrT
      ; forall(arg(AP, Into, Arg),
               ( term_priority_gnd(Into, M, AP, PrA),
                 \+ term_needs_braces(M:Arg, PrA)
               ))
      )
    ; option(module(M), Options),
      \+ current_op(_, _, M:FT),
      \+ current_op(_, _, M:FP)
    ),
    ( FT == FP
    ->NT = FT % preserve layout
    ; NT = '$TEXTQ'(FT)
    ),
    !,
    mapilist([Into, Term] +\ N^Pos^(PosK-v(N, Pos, Arg, TAr))^
            ( arg(N, Into, Arg),
              arg(N, Term, TAr),
              normalize_pos(Pos, PosK)
            ), 1, PosT, KPosValTU),
    /* 0 is the functor, priority 1200 */
    KPosValU = [(FFrom-FFTo)-v(0, FFrom-FFTo, NT, FP)|KPosValTU],
    keysort(KPosValU, KPosValL),
    pairs_keys_values(KPosValL, PosKL, ValL),
    from_to_pairs(PosKL, From, To1, To2, FromToL, []),
    succ(A, N),
    nth1(N, PosKL, E),
    arg(2, E, To2),
    print_subtext(Text, From, To1),
    foldil(print_expansion_arg(M, Into, Options, Text, A), 1, FromToL, ValL, _, true),
    print_subtext(Text, To2, To).
print_expansion_pos(sub_list_position(BFrom, To, BTo, From, PosL, Tail), Into, Term, Options, Text) :-
    print_subtext(Text, BFrom, BTo),
    print_expansion_list(PosL, From, To, Tail, Into, Term, Options, Text, init).
print_expansion_pos(list_position(From, To, PosL, Tail), Into, Term, Options, Text) :-
    print_expansion_list(PosL, From, To, Tail, Into, Term, Options, Text, init).
print_expansion_pos(brace_term_position(From, To, TermPos), {Into}, {Term}, Options1, Text) :-
    arg(1, TermPos, AFrom),
    arg(2, TermPos, ATo),
    print_subtext(Text, From, AFrom),
    merge_options([priority(1200)], Options1, Options),
    print_expansion_elem(Options, Text, 1, 1, ATo-To, TermPos, Into, Term, _, true).
print_expansion_pos(parentheses_term_position(From, To, TermPos), Into, Term, Options1, Text) :-
    arg(1, TermPos, AFrom),
    arg(2, TermPos, ATo),
    print_subtext(Text, From, AFrom),
    merge_options([priority(1200)], Options1, Options),
    print_expansion_elem(Options, Text, 1, 1, ATo-To, TermPos, Into, Term, _, true).

print_expansion_list(PosL, From, To, TPos, IntoL, TermL, Options1, Text, Cont) :-
    ( ( IntoL = '$sb'(sub_list_position(_, To2, _, From2, PosL2, TPos2), _, RepL, Priority, Into),
        PosL = [Pos|_],
        arg(1, Pos, From1)
      ->print_subtext(Text, From, From1)
      ; IntoL = '$sb'(list_position(From21, To2, PosL2, TPos2), _, RepL, Priority, Into),
        ( Cont = cont,
          PosL2 = [Pos2|_],
          compound(Pos2),
          arg(1, Pos2, From2)
        ->write(', ')
        ; From2 = From21
        )
      )
    ->print_subtext_sb_2(Into, list_position(From2, To2, PosL2, TPos2), RepL, Priority, Text, Options1)
    ; ( PosL = [Pos|PosT]
      ->( normalize_pos(Pos, From1-To1),
          IntoL = [Into|IT],
          TermL = [Term|TT]
        ->option(module(M), Options1),
          term_priority([_|_], M, 1, Priority1),
          select_option(priority(Priority), Options1, Options, Priority),
          Options2=[priority(Priority1)|Options],
          print_subtext(Text, From, From1),
          print_expansion(Into, Term, Pos, Options2, Text),
          print_expansion_list(PosT, To1, To, TPos, IT, TT, Options1, Text, cont)
        ; IntoL = []
        ->arg(1, Pos, From1),
          last(PosL, LPos),
          arg(2, LPos, To1),
          ( Cont = cont
          ->true
          ; print_subtext(Text, From, From1)
          ),
          print_subtext(Text, To1, To)
        )
      )
    ->true
    ; PosL = []
    ->( TPos = none
      ->( IntoL == []
        ->print_subtext(Text, From, To)
        ; print_expansion(IntoL, TermL, From-To, Options1, Text)
        )
      ; normalize_pos(TPos, From1-To1),
        print_subtext(Text, From, From1),
        print_expansion(IntoL, TermL, TPos, Options1, Text),
        print_subtext(Text, To1, To)
      )
    ; write_term(IntoL, Options1)
    ).

print_subtext(RefPos, Text) :-
    get_subtext(RefPos, Text, SubText),
    print_text(SubText).

print_text(Text) :- format("~s", [Text]), write(''). % reset partial(true) logic

print_subtext(Text, From, To) :-
    get_subtext(Text, From, To, SubText),
    print_text(SubText).

get_subtext(RefPos, Text, SubText) :-
    compound(RefPos),
    arg(1, RefPos, From),
    arg(2, RefPos, To),
    get_subtext(Text, From, To, SubText).

get_subtext(Text1, From, To, Text) :-
    arithexpression(From),
    arithexpression(To),
    LPaste is To-From,
    From1 is max(0, From),
    sub_string(Text1, From1, LPaste, _, Text).

bin_op(Term, Op, Left, Right, A, B) :-
    nonvar(Term),
    functor(Term, Op, N),
    N == 2,
    prolog_listing:infix_op(Op, Left, Right),
    arg(1, Term, A),
    arg(2, Term, B).

rportray_bodyb(B, Pos, OptL) :- write_b(B, OptL, Pos).

rportray_body(B, Pos, OptL) :- write_b1(B, OptL, Pos).

write_b(Term, OptL, Pos1) :-
    ( option(priority(N), OptL),
      option(module(M), OptL),
      term_needs_braces(M:Term, N)
    ->stream_property(current_output, position(S1)),
      write_t('( ', OptL),
      stream_property(current_output, position(S2)),
      stream_position_data(char_count, S1, B1),
      stream_position_data(char_count, S2, B2),
      Pos is Pos1+B2-B1,
      write_b1(Term, OptL, Pos),
      nl,
      line_pos(Pos - 2),
      write_t(')', OptL)
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
    mapnargs(body_meta_arg, Term, Spec, Meta).

ctrl((_ ,   _)).
ctrl((_ ;   _)).
ctrl((_ ->  _)).
ctrl((_ *-> _)).

skip_format(_/_).
skip_format('$VAR'(_)).
skip_format(_:_).

compact_format(_-_).

body_meta_arg(_, Term, Spec, Meta) :-
    ( Spec = 0,
      nonvar(Term)
    ->Meta = '$BODYB'(Term)
    ; Meta = Term
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
    LinePos1 is LinePos - 1,
    line_pos(LinePos1).
line_pos(_) :- write('').

write_t(Term, Options1) :-
    write_qt(false, Term, Options1).

write_q(Term, Options1) :-
    write_qt(true, Term, Options1).

write_qt(Quoted, Term, Options1) :-
    merge_options([quoted(Quoted), priority(1200)], Options1, Options2),
    select_option(portray_goal(PG), Options2, Options, PG),
    write_term(Term, Options).
