:- module(keep_comments, [f/3]).

f(a, f(/*1*/f( a)), f(/*2*/f( f(a  )))).
