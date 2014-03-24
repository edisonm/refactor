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

Sorry for the lack of documentation, but this library is still in alfa
status and changing a lot,  sometimes even without advice. However, in
the  file tests/refactor.plt  you can  see useful  examples about  the
usage of this library.  Some examples follows:



```prolog
?- cd(tests).
true.

?- ['refactor.plt'].
%  comment_data compiled into comment_data 0.00 sec, 6 clauses
% refactor.plt compiled 0.04 sec, 526 clauses
true.

?- run_tests.
% All 24 tests passed
true.
?- rreset.
true.

?- replace_term(conjex:_,a(B),aa(B),[]).
% 2 changes in /home/edison/apps/refactor/tests/conjex.pl
--- conjex.pl (source)
+++ conjex.pl (target)
@@ -1,10 +1,10 @@
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
% Saved changes in index 1
true.


```
