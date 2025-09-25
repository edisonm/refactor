:- module(tags_dict, [test1/0]).

test1 :-
    call(first,
         [ f(o{b:1,
               a:2}
            )
         ]).

test2 :-
    call(first,
         [ f(_{b:1,
               a:2}
            )
         ]).

