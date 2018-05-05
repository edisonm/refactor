:- begin_tests(refactor).

:- include(refactor_common).
:- use_module(library(clambda)).

/* $bind1$
diff -ruN bind1.pl -
--- bind1.pl (source)
+++ bind1.pl (target)
@@ -1,4 +1,4 @@
 
-f([[a, B, ""], B]).
+[["a", B, ""], B].
 
-f([['1', ""], '']).
+[["1", ""], ""].
*/
test(bind1) :- % tests the need of collapse the bindings
    execute_test(bind1,
                 replace_sentence(f(Text),$@(Text4),
                                  ( substitute(\ X^XS
                                              ^( atomic(X),
                                                 X \= []
                                               ->atom_string(X, XS)
                                               ), Text, Text3),
                                    copy_term(Text3, Text4),
                                    Text4=Text3
                                  )),
                 [file(bind1)]).

:- end_tests(refactor).
