:- module(keep_list_layout, [keep_list_layout/2]).

keep_list_layout([A|_B],A).
keep_list_layout([A,_B],A).
keep_list_layout({A,_B},A).
keep_list_layout((A,_B),A).
