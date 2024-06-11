:- use_module(library(clpfd)).

:- begin_tests(clpfd_tutorial).

example1(X, Y) :-
  X in 0..10,
  Y in 4..8,
  X #> Y.

% ?- example1(X, Y).
% X in 5..10,
% Y#=<X+ -1,
% Y in 4..8.

example2(X, Y) :-
  X in 0..5,
  Y in 4..8,
  X #> Y.

test(example2_X_is_5_Y_is_4) :-
  example2(X, Y),
  assertion(X =:= 5),
  assertion(Y =:= 4).

:- end_tests(clpfd_tutorial).
:- run_tests.

foo(X) :- X in 1..3 .
foo(X) :- X in 5..7 .
foo(X) :- X in 8..12 .

% ?- trace.
% true.

% [trace]  ?- foo(6).
%    Call: (10) foo(6) ? creep
%    Call: (11) integer(6) ? creep
%    Exit: (11) integer(6) ? creep
%    Call: (11) between(1, 3, 6) ? creep
%    Fail: (11) between(1, 3, 6) ? creep
%    Redo: (10) foo(6) ? creep
%    Call: (11) integer(6) ? creep
%    Exit: (11) integer(6) ? creep
%    Call: (11) between(5, 7, 6) ? creep
%    Exit: (11) between(5, 7, 6) ? creep
%    Exit: (10) foo(6) ? creep
% true ;
%    Redo: (10) foo(6) ? creep
%    Call: (11) integer(6) ? creep
%    Exit: (11) integer(6) ? creep
%    Call: (11) between(8, 12, 6) ? creep
%    Fail: (11) between(8, 12, 6) ? creep
%    Fail: (10) foo(6) ? creep
% false.

% [trace]  ?- nodebug.
% true.