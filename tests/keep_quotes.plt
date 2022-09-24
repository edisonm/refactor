:- begin_tests(keep_quotes).

:- include(refactor_common).

:- use_module(keep_quotes).

/* $keep_quotes$
diff -ruN keep_quotes.pl -
--- keep_quotes.pl (source)
+++ keep_quotes.pl (target)
@@ -1,3 +1,3 @@
 :- module(keep_quotes, ['keep_quotes'/0]).
 
-keep_quotes :- display('keep_quotes').
+keep_quotes :- keep_quotes, 'keep_quotes', display('keep_quotes').
*/

test(keep_quotes) :-
    execute_test(keep_quotes, replace_sentence((A :- display(B)),
                                       (A :- A, B, display(B)), true),
                [module(keep_quotes), linearize([atms])]).

:- end_tests(keep_quotes).
