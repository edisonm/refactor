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

Usage
=====

Although the documentation is still not complete, due its alfa status and still
changing, there are some tests in tests/ where you can see useful examples about
its usage.  Some examples follows:


```prolog
?- [library(refactor)].
true.

?- cd(tests).
true.

?- replace_term(a(B), aa(B), [file(repl_conj)]).
% 3 changes of 3 attempts
% Saved changes in index 1
true.
```

To review the changes, use rshow:

```prolog
?- rshow.
diff -ruN repl_conj.pl -
--- repl_conj.pl (source)
+++ repl_conj.pl (target)
@@ -1,15 +1,15 @@
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
 
-a(_).
+aa(_).
 b(_).
 c(_).
 d(_).
true.

```

To undo the changes, use rreset, and to accept them use rcommit.  Continuous
calls to refactor predicates can be stacked so you could implement complex
scenarios via small ones.  Some refactoring scenarios that results of the
composition of multiple changes are implemented in the library ref_scenarios.pl.

Other complex scenarios are implemented in its own modules, as follows:

- deref_reexport.pl helps to replace reexport declarations by use_module declarations

- file_to_module.pl convert included files to modules

- move_preds.pl helps to move predicates from one file to another


Papers
======

Please, before to read it, be aware that this paper is provided here for
historical reasons, since the implementation and interface have changed quite a
lot.

Edison Mera, Jan Wielemaker: Porting and refactoring Prolog programs:
the PROSYN case study. TPLP 13(4-5-Online-Supplement) (2013)

http://www.swi-prolog.org/download/publications/prosyn.pdf
