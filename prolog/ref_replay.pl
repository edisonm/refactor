:- module(ref_replay,
	  [rcommit/0,
	   rlist/0,
	   rlist/1,
	   rrewind/0,
	   rrewind/1,
	   rreset/0,
	   rundo/0,
	   rdelete/1,
	   apply_command_q/1
	  ]).

:- use_module(library(ref_shell)).
:- use_module(library(ref_changes),
	      [reset_changes/0,
	       undo_changes/1]).
:- use_module(library(ref_command)).

:- meta_predicate apply_command_q(0).
apply_command_q(Call) :-
    apply_command(Call),
    rdiff_q.

rdiff_q :-
    ( current_prolog_flag(verbose, silent)
    ->true
    ; once(rdiff(Index)),
      print_message(informational, format('Saved changes in index ~w', [Index]))
    ).

rcommit :-
    ref_commit,
    reset_commands.

rlist :-
    \+ ( rlist(_),
	 fail
       ).

rlist(Index) :-
    pending_command(Index, Command),
    with_output_to(string(SCommand), portray_clause(Command)),
    print_message(information, format('Index ~w, Command: ~s', [Index, SCommand])).

rrewind :-
    rrewind(0).

rrewind(Index) :-
    findall(Command, ( pending_command(Index0, Command),
		       Index0 > Index,
		       rdrop(Index0, _)
		     ),
	    CommandR),
    reverse(CommandR, CommandL),
    maplist(apply_command, CommandL).


rundo :-
    rundo(_).

rdelete(Index) :-
    rundo(Index),
    rrewind(Index).

rundo(Index) :-
    rdrop(Index, Command),
    print_message(information, format('Undone ~w ---> ~w', [Index, Command])).

rdrop(Index, Command) :-
    undo_changes(Index),
    undo_command(Index, Command).

rreset :-
    reset_changes,
    reset_commands.