:- module(term_with_op, [term_with_op/1]).

term_with_op(A) :-
    A = /****/ key_components/4+ (/*1*/ ( hidden ), ( kbmask([+, +, -, -]) ) /*2*/).
