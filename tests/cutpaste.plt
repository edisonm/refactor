:- begin_tests(cutpaste).

:- include(refactor_common).

/* $cutpaste$
diff -ruN cutpaste.pl -
--- cutpaste.pl (source)
+++ cutpaste.pl (target)
@@ -1,6 +1,4 @@
-cutpaste(aaa, Arg) :-
-    body(aaa, Arg).
 cutpaste(Key, Arg) :-
-    member(Key, [bbb]),
+    member(Key, [aaa, bbb]),
     neck,
     body(Key, Arg).
*/

test(cutpaste) :-
    execute_test(cutpaste, replace_sentence(
                               [(cutpaste(Key1, Arg1) :-
                                     body(Key1, Arg1)),
                                (cutpaste(Key, Arg) :-
                                     member(Key, KeyL),
                                     neck,
                                     body(Key, Arg))],
                               [(cutpaste(Key, Arg) :-
                                     member(Key, [Key1|KeyL]),
                                     neck,
                                     body(Key, Arg))]
                           ), [file(cutpaste)]).
:- end_tests(cutpaste).
