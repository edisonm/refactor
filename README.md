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

Although this  library lacks  documentation, due  its alfa  status and
still changing, there  are some tests in  tests/refactor.plt where you
can see useful examples about its usage.  Some examples follows:


```prolog
?- cd(tests).
true.

?- ['refactor.plt'].
% refactor.plt compiled 0.04 sec, 526 clauses
true.

?- run_tests.
% All 43 tests passed
true.
?- rreset.
true.

?- replace_term(a(B),aa(B),[module(conjex)]), rshow.
% Reading tests/conjex.pl
% 3 changes in tests/conjex.pl
% Saved changes in index 1
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -1,15 +1,15 @@
 :- module(conjex, [conjex/0]).
 
 conjex :-
-    a(C),
+    aa(C),
     b(b),
     c(C),
     d(d).
 conjex :-
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

Papers
======

Edison Mera, Jan Wielemaker: Porting and refactoring Prolog programs:
the PROSYN case study. TPLP 13(4-5-Online-Supplement) (2013)
http://journals.cambridge.org/downloadsup.php?file=/tlp2013003.pdf

