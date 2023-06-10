refactor
========

Refactoring Tools for SWI-Prolog

Installation
============

To install the refactoring tools, just follow the next sequence of commands
in your SWI-Prolog shell:

```prolog
  $ swipl
  
  ?- pack_install('https://github.com/edisonm/refactor.git').
  true.
```

How it works
============

There are two groups of predicates, one to rewrite the source code, and other
one to manage such changes.  The basic predicate that performs the
transformation is replace/5, implemented in library(ref_replace).  The
predicates to manage such transformations are implemented in library(ref_shell),
which provides methods to keep track of the modifications and to make the
changes to the files permanent.  To make things easy, you can download all the
required libraries by loading library(refactor).

Example of Usage
================

This is more clear with an example.  First load the library:

```prolog
?- [library(refactor)].
true.
```

In the folder tests/ you can see useful examples about its usage, let's pick from
there the module repl_conj.pl, and let's replace the term a(B) by aa(B):

```prolog
?- cd(tests).
true.

?- replace_term(a(B), aa(B), [file(repl_conj)]).
% 3 changes of 3 attempts
% Saved changes in index 1
true.
```

In this example we use the options argument to say that we want to apply the
changes to the file repl_conj.pl, but we can use other ways to define the scope
of the chages, like the directory (dir option), list of directories (dirs
option) and list of files (files option) to mention a few.

The last information message shows in the first line the number of changes
performed out of the number of attempts, and in the second one, the index the
changes belong to, which is used in some predicates like rdiff/1 and will be
explained later.  The number of changes will not match with the number of
attempts if the expansion was rejected by the expander, for instance, suppose
that we want to perform such change in all places except in those where the
clause doesn't have body:

```prolog
?- replace_term(a(B), aa(B), (Sentence = (_ :- _)), [file(repl_conj), sentence(Sentence)]).
% 2 changes of 3 attempts
% Saved changes in index 1
true.
```

To review the changes, use rshow:


```prolog
?- rshow.
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,12 +1,12 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 repl_conj :-
-    a(a),
+    aa(a),
     b(b).
 
 a(_).
true.

```

If we don't agree with the changes, execute rreset to undo them, otherwise execute rcommit:

```prolog
?- rcommit.
true.

```

Continuous calls to refactor predicates can be stacked so you could implement
complex scenarios via small ones.  For instance, suppose you also want to change
b(C) by bb(C) and c(DD) by cc(DD), then first we execute rreset just to be sure no
changes are pending folowed with the calls to perform the changes:

```prolog
?- rreset.
true.

?- replace_term(a(B), aa(B), [file(repl_conj), sentence(Sentence)]).
% 3 changes of 3 attempts
% Saved changes in index 1
true.

?- replace_term(b(B), bb(B), [file(repl_conj), sentence(Sentence)]).
% 3 changes of 3 attempts
% Saved changes in index 2
true.

?- replace_term(c(D), cc(D), [file(repl_conj), sentence(Sentence)]).
% 2 changes of 2 attempts
% Saved changes in index 3
true.

?- rshow.
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,15 +1,15 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
-    b(b),
-    c(C),
+    aa(C),
+    bb(b),
+    cc(C),
     d(d).
 repl_conj :-
-    a(a),
-    b(b).
+    aa(a),
+    bb(b).
 
-a(_).
-b(_).
-c(_).
+aa(_).
+bb(_).
+cc(_).
 d(_).
true.

```

As soon as the refactorings become more complex or touch more files, it results
difficult to use only rshow.  In such cases it's more convenient to save the
differencies in a diff file:

```prolog
?- rsave('changes.diff').
true.

```

Be aware that if before to call rcommit we modify some files by hand, those
changes will be overwritten, therefore the refactoring needs to be applied
again.  That can be performed easily with the command rrewind:

```prolog
?- rrewind.
% 3 changes of 3 attempts
% 3 changes of 3 attempts
% 2 changes of 2 attempts
true.

```

You can check each one of the changes with rdiff(Index), where Index is a number
to refer to the given change.  If Index is uninstantiated, rdiff/1 will show all
the changes via backtracking. For instance:

```prolog
?- rdiff(X).
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -3,7 +3,7 @@
 repl_conj :-
     aa(C),
     bb(b),
-    c(C),
+    cc(C),
     d(d).
 repl_conj :-
     aa(a),
@@ -11,5 +11,5 @@
 
 aa(_).
 bb(_).
-c(_).
+cc(_).
 d(_).
X = 3 ;
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -2,14 +2,14 @@
 
 repl_conj :-
     aa(C),
-    b(b),
-    c(C),
+    bb(b),
+    cc(C),
     d(d).
 repl_conj :-
     aa(a),
-    b(b).
+    bb(b).
 
 aa(_).
-b(_).
-c(_).
+bb(_).
+cc(_).
 d(_).
X = 2 ;
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,15 +1,15 @@
 :- module(repl_conj, [repl_conj/0]).
 
 repl_conj :-
-    a(C),
-    b(b),
-    c(C),
+    aa(C),
+    bb(b),
+    cc(C),
     d(d).
 repl_conj :-
-    a(a),
-    b(b).
+    aa(a),
+    bb(b).
 
-a(_).
-b(_).
-c(_).
+aa(_).
+bb(_).
+cc(_).
 d(_).
X = 1.

```

Or for a specific index:

```prolog

?- rdiff(2).
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -2,14 +2,14 @@
 
 repl_conj :-
     aa(C),
-    b(b),
-    c(C),
+    bb(b),
+    cc(C),
     d(d).
 repl_conj :-
     aa(a),
-    b(b).
+    bb(b).
 
 aa(_).
-b(_).
-c(_).
+bb(_).
+cc(_).
 d(_).
true.

```

Some refactoring scenarios that result from the composition of multiple changes
are implemented in the library ref_scenarios.pl.

Other complex scenarios are implemented in its own modules, as follows:

- deref_reexport.pl helps to replace reexport declarations by use_module declarations

- file_to_module.pl convert included files to modules

- move_preds.pl helps to move predicates from one file to another

```prolog

% TBD: Document the next steps to make move_preds.pl work, but note that this
% example runs under plsteroids, not rtchecks:

?- [loadall].
?- [library(calls_to)].
?- collect_calls_to([dir('.')], _).
?- [library(module_links)].
?- [library(move_preds)].
?- update_depends_of.

% rreset,
% move_preds([put_mark/1], stchecks/prolog/check_unused, xtools/prolog/mark_preds, []),
% rsave('ps.diff').

```

Papers
======

Please, before to read it, be aware that this paper is provided here for
historical reasons, since the implementation and interface have changed quite a
lot.

Edison Mera, Jan Wielemaker: Porting and refactoring Prolog programs:
the PROSYN case study. TPLP 13(4-5-Online-Supplement) (2013)

http://www.swi-prolog.org/download/publications/prosyn.pdf
