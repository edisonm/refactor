:- begin_tests(collapse_bindings).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $collapse_bindings$
diff -ruN collapse_bindings.pl -
--- collapse_bindings.pl (source)
+++ collapse_bindings.pl (target)
@@ -1,4 +1,4 @@
 
-f([[a, B, ""], B]).
+[["a", B, ""], B].
 
-f([['1', ""], '']).
+[["1", ""], ""].
*/
test(collapse_bindings) :- % tests the need of collapse the bindings
    execute_test(collapse_bindings,
                 replace_sentence(f(Text),$@(Text4),
                                  ( substitute(\ X^XS
                                              ^( atomic(X),
                                                 X \= []
                                               ->atom_string(X, XS)
                                               ), Text, Text3),
                                    copy_term(Text3, Text4),
                                    Text4=Text3
                                  )),
                 [file(collapse_bindings)]).

:- end_tests(collapse_bindings).
