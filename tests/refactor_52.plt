:- begin_tests(refactor_52).

:- include(refactor_common).
:- user:use_module(library(solution_sequences)).

/* $meta1$
diff -ruN meta1.pl -
--- meta1.pl (source)
+++ meta1.pl (target)
@@ -2,14 +2,15 @@
 
 p(L, L2, L3) :-
     findall(E,
-            distinct(E,
-               ( member(E, L),
-                 E = ~a,
-                \+((member(E, L2),
-                    member(E, L3)
-                  )),
-                (once(sub_string(E, _, _, _, a))
-                ;once(sub_string(E, _, _, _, b))
-                )
-              )), RL),
+            order_by(asc(E),
+               distinct(E,
+                        ( member(E, L),
+                          E = ~a,
+                          \+ ( member(E, L2),
+                               member(E, L3)
+                             ),
+                          ( once(sub_string(E, _, _, _, a))
+                          ; once(sub_string(E, _, _, _, b))
+                          )
+                        ))), RL),
     writeln(user_output, RL).
*/

test(meta1) :-
    execute_test(meta1,
                 replace(body_rec,
                         findall(E, G, L),
                         findall(E, order_by(asc(E),
                                             $@('$BODYB'(G2))), L),
                         ( duplicate_term(G, G2),
                           G2=G
                         )), [file(meta1)]).

:- end_tests(refactor_52).
