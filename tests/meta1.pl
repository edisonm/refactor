:- use_module(library(i18n/i18n_op)).

p(L, L2, L3) :-
    findall(E,
            distinct(E,
               ( member(E, L),
                 E = ~a,
                \+((member(E, L2),
                    member(E, L3)
                  )),
                (once(sub_string(E, _, _, _, a))
                ;once(sub_string(E, _, _, _, b))
                )
              )), RL),
    writeln(user_error, RL).
