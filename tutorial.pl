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

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
  Vars = [S,E,N,D,M,O,R,Y],
  Vars ins 0..9,
  all_different(Vars),
            S*1000 + E*100 + N*10 + D +
            M*1000 + O*100 + R*10 + E #=
  M*10000 + O*1000 + N*100 + E*10 + Y,
  M #\= 0, S #\= 0.

test(puzzle, all(As=[[9, 5, 6, 7]])) :-
  puzzle(As+Bs=Cs), label(As),
  assertion(As ==    [9, 5, 6, 7]),
  assertion(Bs ==    [1, 0, 8, 5]),
  assertion(Cs == [1, 0, 6, 5, 2]).

test(removing_symmetry) :-
  Vs = [A,B,C,D], Vs ins 1..4,
  all_different(Vs),
  A #< B, C #< D, A #< C,
  findall(pair(A,B)-pair(C,D), label(Vs), Ms),
  assertion(Ms == [pair(1, 2)-pair(3, 4), pair(1, 3)-pair(2, 4), pair(1, 4)-pair(2, 3)]).

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

puzzle2([S,U,N] + [F,U,N] = [S,W,I,M]) :-
  Vars = [F,U,N,S,W,I,M],
  Vars ins 0..9,
  all_different(Vars),
           S*100 + U*10 + N +
           F*100 + U*10 + N #=
  S*1000 + W*100 + I*10 + M,
  S #\= 0, F #\= 0.

% ?- puzzle2(As+Bs=Cs), label(As).
% As = [1, 2, 3],
% Bs = [9, 2, 3],
% Cs = [1, 0, 4, 6] ;
% As = [1, 2, 7],
% Bs = [9, 2, 7],
% Cs = [1, 0, 5, 4] ;
% As = [1, 2, 8],
% Bs = [9, 2, 8],
% Cs = [1, 0, 5, 6] ;
% As = [1, 3, 2],
% Bs = [9, 3, 2],
% Cs = [1, 0, 6, 4] ;
% As = [1, 3, 4],
% Bs = [9, 3, 4],
% Cs = [1, 0, 6, 8] ;
% As = [1, 3, 6],
% Bs = [9, 3, 6],
% Cs = [1, 0, 7, 2] ;
% As = [1, 3, 8],
% Bs = [9, 3, 8],
% Cs = [1, 0, 7, 6] ;
% As = [1, 4, 3],
% Bs = [9, 4, 3],
% Cs = [1, 0, 8, 6] ;
% As = [1, 6, 7],
% Bs = [8, 6, 7],
% Cs = [1, 0, 3, 4] ;
% As = [1, 7, 3],
% Bs = [8, 7, 3],
% Cs = [1, 0, 4, 6] ;
% As = [1, 7, 6],
% Bs = [8, 7, 6],
% Cs = [1, 0, 5, 2] ;
% false.

increase([]).
increase([_]).
increase([A, B | T]) :-
  A #< B,
  increase(T).

% ?- increase([1, X, 3]).
% X in 2..sup ;
% false.

% ?- increase([1, X, 4]).
% X in 2..sup ;
% false.

% ?- increase([1, X, Y, 4]).
% X in 2..sup,
% Y in inf..3.

% ?- increase([1, 2]).
% true.

% ?- increase([1, X]).
% X in 2..sup.
