:- begin_tests(replace_sentences).

:- include(refactor_common).

:- use_module(replace_sentences).

/* $replace_sentences$
diff -ruN replace_sentences.pl -
--- replace_sentences.pl (source)
+++ replace_sentences.pl (target)
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

test(replace_sentences) :-
    execute_test(replace_sentences, replace_sentence([f(a),f(b)],[f(b),f(a),f(d)])).

/* $replace_sentences2$
diff -ruN replace_sentences.pl -
--- replace_sentences.pl (source)
+++ replace_sentences.pl (target)
@@ -1,11 +1,6 @@
 :- module(replace_sentences, [f/1]).
 
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
test(replace_sentences2) :-
    execute_test(replace_sentences2, replace_sentence([p(A),p(B)],p(C),flatten([A,B],C)),[file(replace_sentences), fixpoint(true)]).

:- end_tests(replace_sentences).
