:- module(follow_layout, [follow_layout/2, follow_layout/3]).

follow_layout(_C, (2,3)).

follow_layout(f(/*1*/A, 0 ), _B, f(/*2*/b, f(/*3*/A, 0 ))).

follow_layout([1|C], C, [2,3]).

follow_layout([/*1*/A], _B, [/**/b, A]).
