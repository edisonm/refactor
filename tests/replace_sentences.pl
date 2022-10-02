:- module(replace_sentences, [f/1]).

% proper merging:

p(a).
p(b).
p(c).
p(d).

% Hello world
% This comment belongs to f(a)

f(a).

% more

% There we go
f(b).

% don't delete this.
