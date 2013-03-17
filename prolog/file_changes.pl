:- module(file_changes, [do_file_change/2, do_file_changes/2]).

do_file_changes(Action, FileChanges) :-
	maplist(do_file_change(Action), FileChanges).

do_file_change(save, File-Changes) :-
    ( \+ access_file(File, exist), Changes==[] -> true
    ; open(File, write, Fd, []),
      format(Fd, '~s', [Changes]),
      close(Fd)
    ).
do_file_change(show, File-Changes) :-
	diff_file_change([], File-Changes).
do_file_change(diff(DiffFile), File-Changes) :-
	% (exists_file(DiffFile) -> delete_file(DiffFile) ; true),
	diff_file_change([' >> ', DiffFile], File-Changes).

make_relative(File, RFile) :-
	( absolute_file_name('',WD),
	  atom_concat(WD, RFile, File) -> true
	; RFile = File
	).
    
diff_file_change(ExtraOptions, File-Changes) :-
	TmpFile = '/tmp/diff_file_change.tmp',
	open(TmpFile, write, Fd, []),
	format(Fd, '~s', [Changes]),
	close(Fd),
	make_relative(File, RFile),
	atomic_list_concat(['diff -ruN',
			    ' --label "', RFile, ' (source)" ', File,
			    ' --label "', RFile, ' (target)" ', TmpFile
			   | ExtraOptions], Command),
	shell(Command, _),
	delete_file(TmpFile).
