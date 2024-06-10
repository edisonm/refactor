:- begin_tests(bodyb_layout).

:- include(refactor_common).
:- user:use_module(library(solution_sequences)).

/* $bodyb_layout$
diff -ruN bodyb_layout.pl -
--- bodyb_layout.pl (source)
+++ bodyb_layout.pl (target)
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
+                     distinct(E,
+                              ( member(E, L),
+                                E = ~a,
+                                \+ ( member(E, L2),
+                                     member(E, L3)
+                                   ),
+                                ( once(sub_string(E, _, _, _, a))
+                                ; once(sub_string(E, _, _, _, b))
+                                )
+                              ))), RL),
     writeln(user_output, RL).
*/

test(bodyb_layout) :-
    execute_test(bodyb_layout,
                 replace(body_rec,
                         findall(E, G, L),
                         findall(E, order_by(asc(E),
                                             $@('$BODYB'(G2))), L),
                         ( duplicate_term(G, G2),
                           G2=G
                         )), [file(bodyb_layout)]).

:- end_tests(bodyb_layout).
