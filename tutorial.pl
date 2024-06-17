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

chem_tank(Temp, Startup) :-
  Temp #< 800,
  #\ Startup #==> Temp #> 300.

chem_demo(Temp, TimeNow, StartTime) :-
  chem_tank(Temp, TimeNow - StartTime #< 10).

test(chem_demo_startup_valid_temp) :-
  assertion(chem_demo(299, 2, 1)).

test(chem_demo_startup_invalid_temp) :-
  assertion(\+ chem_demo(801, 2, 1)).

test(chem_demo_startup_valid_temp) :-
  assertion(chem_demo(301, 11, 1)).

test(chem_demo_startup_invalid_temp) :-
  assertion(\+ chem_demo(801, 11, 1)).

% ?- length(List, 4),List ins 1..4, all_different(List), List = [1,_,3,4].
% List = [1, 2, 3, 4].

% ?- X in 1..2, Y in 1..2, Z in 1..2, all_different([X,Y,Z]).
% X in 1..2,
% all_different([X, Y, Z]),
% Y in 1..2,
% Z in 1..2.

% ?- X in 1..2, Y in 1..2, Z in 1..2, all_distinct([X,Y,Z]).
% false.

test(all_distinct_impossible_values) :-
  X in 1..2, Y in 1..2, Z in 1..2,
  assertion(\+ all_distinct([X,Y,Z])).

trains([
  % from station, to station, departs at, arrives at
  [1,2,0,1],
  [2,3,4,5],
  [2,3,0,1],
  [3,4,5,6],
  [3,4,2,3],
  [3,4,8,9]]).

threepath(A, D, Ps) :-
  Ps = [[A,B,_T0,T1],[B,C,T2,T3],[C,D,T4,_T5]],
  T2 #> T1,
  T4 #> T3,
  trains(Ts),
  tuples_in(Ps, Ts).

test(threepath_1_4) :-
  threepath(1, 4, Ps),
  assertion(Ps == [[1, 2, 0, 1], [2, 3, 4, 5], [3, 4, 8, 9]]).

% npath(A, D, Ps) :- npath(A, D, [], Ps), trains(Ts), tuples_in(Ps, Ts).
% npath(D, D, Ps, Ps).
% npath(A, D, Ps0, Ps) :-
%   A #< D,
%   append(Ps0, [[A,B,_T0,T1], [B,C,T2,_T3]], Ps1),
%   T2 #> T1,
%   npath(C, D, Ps1, Ps).

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

% ?- length(X, 5).
% X = [_, _, _, _, _].

% ?- length(X, Y).
% X = [],
% Y = 0 ;
% X = [_],
% Y = 1 ;
% X = [_, _],
% Y = 2 ;
% X = [_, _, _],
% Y = 3 ;
% X = [_, _, _, _],
% Y = 4 ;
% X = [_, _, _, _, _],
% Y = 5 .

/*
	 much shorter quarreling children
   https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#114-sudoku

	 16 children are to be seated in a
	 4 x 4 array of chairs.

         the children are 8 girls (numbered 1..8) and
	 8 boys (numbered 9..16).

     1,3,5,8 think boys are ucky
	 9,10,11,14 think girls are gross

	 these pairs are enemies

	 [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]

 */

 length_(Length, List) :- length(List, Length).

 child_row(X) :- X ins 1..16 .
 
 ww(X) :-
   write(X),
   write('/').
 
 print_row(Row) :-
   maplist(ww, Row),
   nl.
 
 children(Class) :-
   length(Class, 4),
   maplist(length_(4), Class),
   maplist(child_row , Class),
   maplist(row_compatible, Class),
   transpose(Class, TransClass),
   maplist(row_compatible, TransClass),
   flatten(Class, FlatClass),
   all_different(FlatClass),
   maplist(label, Class),
   maplist(print_row, Class).
 
 row_compatible([A,B,C,D]) :-
   compatible(A, B),
   compatible(B, C),
   compatible(C, D).
 
 compatible(A, B) :-
   not_enemy(A, B),
   not_enemy(B, A),
   sex_compatible(A, B),
   sex_compatible(B, A).
 
 not_enemy(A, B) :-
   NotA #\= A #\/ NotB #\= B,
   tuples_in([[NotA, NotB]],
         [[1,2], [4,6], [4,7], [4, 9],[9,11], [12, 14], [14,16]]).
 
 sex_compatible(A, B) :-
   A in 1\/3\/5\/8 #==> B #=< 8,
   A in  9..11\/14 #==> B #> 8.

  % ?- children(Class).
  % 1/2/3/5/
  % 4/12/6/8/
  % 13/9/15/7/
  % 10/11/14/16/
  % Class = [[1, 2, 3, 5], [4, 12, 6, 8], [13, 9, 15, 7], [10, 11, 14, 16]] ;
  % 1/2/3/5/
  % 4/12/6/8/
  % 13/9/15/7/
  % 10/14/11/16/
  % Class = [[1, 2, 3, 5], [4, 12, 6, 8], [13, 9, 15, 7], [10, 14, 11, 16]] .