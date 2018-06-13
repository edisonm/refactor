:- begin_tests(refactor_40).

:- include(refactor_common).

:- use_module(exsl).

/* $exsl$
diff -ruN exsl.pl -
--- exsl.pl (source)
+++ exsl.pl (target)
@@ -7,14 +7,14 @@
 p(c).
 p(d).
 
-% Hello world
-% This comment belongs to f(a)
-
-f(a).
-
 % more
 
 % There we go
 f(b).
+% Hello world
+% This comment belongs to f(a)
+
+f(a).
+f(d).
 
 % don't delete this.
*/

test(exsl) :-
    execute_test(exsl, replace_sentence([f(a),f(b)],[f(b),f(a),f(d)])).

/* $exsl2$
diff -ruN exsl.pl -
--- exsl.pl (source)
+++ exsl.pl (target)
@@ -1,11 +1,6 @@
 :- module(exsl, [f/1]).
 
-% proper merging:
-
-p(a).
-p(b).
-p(c).
-p(d).
+p([a, b, c, d]).
 
 % Hello world
 % This comment belongs to f(a)
*/
test(exsl2) :-
    execute_test(exsl2, replace_sentence([p(A),p(B)],p(C),flatten([A,B],C)),[file(exsl), fixpoint(true)]).

:- end_tests(refactor_40).
