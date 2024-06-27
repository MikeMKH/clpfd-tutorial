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

npath(A, D, Ps) :-
  trains(Ts),
  member([A,B,T0,T1], Ts),
  npath(B, D, [[A,B,T0,T1]], Ps).
npath(D, D, Ps, Ps).
npath(B, D, Ps0, Ps) :-
  B #< D,
  last(Ps0, [_A,B,_T0,T1]),
  append(Ps0, [[B,C,T2,T3]], Ps1),
  T2 #> T1,
  trains(Ts),
  member([B,C,T2,T3], Ts),
  npath(C, D, Ps1, Ps).

test(npath_1_4, all(Ps=[[[1, 2, 0, 1], [2, 3, 4, 5], [3, 4, 8, 9]]])) :-
  npath(1, 4, Ps),
  assertion(Ps == [[1, 2, 0, 1], [2, 3, 4, 5], [3, 4, 8, 9]]).

test(threepath_1_4_eq_npath_1_4, all(Ns=[[[1, 2, 0, 1], [2, 3, 4, 5], [3, 4, 8, 9]]])) :-
  npath(1, 4, Ns),
  threepath(1, 4, Ts),
  assertion(Ns == Ts).

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#12-resources
%  penny candy example
%
%  Timmy has 25 cents
%  gumballs cost a penny
%  snickers cost 10 cents
%  toffees are 2 cents
%  licorice costs 5 cents
%
%  what are Timmys alternatives?
%  assume Timmy spends the entire 25 cents
penny_candy_example(Candy) :-
	Candy = [_Gumball, _Snickers, _Toffee, _Licorice],
	Candy ins 0..sup,
	scalar_product([1, 10, 2, 5], Candy, #=, 25),
	label(Candy).

% ?- penny_candy_example([Gumball, Snickers, Toffee, Licorice]).
% Gumball = Snickers, Snickers = Toffee, Toffee = 0,
% Licorice = 5 ;
% Gumball = Snickers, Snickers = 0,
% Toffee = 5,
% Licorice = 3 ;
% Gumball = Snickers, Snickers = 0,
% Toffee = 10,
% Licorice = 1 ;
% Gumball = Toffee, Toffee = 0,
% Snickers = 1,
% Licorice = 3 .
%
% and so ...

test(penny_candy_example_licorice_is_5, all(Licorice = [5])) :-
  penny_candy_example([Gumball, Snickers, Toffee, Licorice]),
  Licorice=5,
  assertion(Licorice =:= 5),
  assertion(Gumball =:= 0),
  assertion(Snickers =:= 0),
  assertion(Toffee =:= 0).

test(penny_candy_example_snickers_is_2_licorice_is_1, all([Snickers, Licorice] = [[2, 1]])) :-
  penny_candy_example([Gumball, Snickers, Toffee, Licorice]),
  Snickers=2,
  Licorice=1,
  assertion(Snickers =:= 2),
  assertion(Licorice =:= 1),
  assertion(Gumball =:= 0),
  assertion(Toffee =:= 0).

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#135-automaton
single_source_automaton(Vs) :-
  automaton(Vs, [source(a), sink(d)],
    [arc(a, 0, b),
     arc(b, 0, b),
     arc(b, 1, c),
     arc(c, 2, d)
    ]).

demo_single_source(Vs) :-
  length(Vs, 4),
  single_source_automaton(Vs),
  label(Vs).

test(demo_single_source) :-
  demo_single_source(Vs),
  assertion(Vs == [0, 0, 1, 2]).

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#135-automaton
multi_source_automaton(Vs) :-
  automaton(Vs, [source(a), source(e), sink(d), sink(h)],
      [arc(a,0,b),
       arc(b,0,b),
       arc(b,1,c),
       arc(c,2,d),
       arc(e,10,f),
       arc(f,10,f),
       arc(f,11,g),
       arc(g,12,h)]).

demo_multi_source_automaton(Vs) :-
  length(Vs, 4),
  multi_source_automaton(Vs),
  label(Vs).

test(demo_multi_source_automaton_path1, all(Vs = [[0,0,1,2]])) :-
  demo_multi_source_automaton(Vs),
  Vs == [0,0,1,2],
  assertion(Vs == [0,0,1,2]).

test(demo_multi_source_automaton_path2, all(Vs = [[10,10,11,12]])) :-
  demo_multi_source_automaton(Vs),
  Vs == [10,10,11,12],
  assertion(Vs == [10,10,11,12]).

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
  
% ?- X in 0..10, Y in 11..20,zcompare(C, X, Y).
% C = (<),
% X in 0..10,
% Y in 11..20.

% ?- X in 0..11, Y in 11..20,zcompare(C, X, Y).
% X in 0..11,
% zcompare(C, X, Y),
% Y in 11..20,
% freeze(C, clpfd:zcompare_(C, X, Y)).

% ?- X in 0..11, Y in 11..20,zcompare(C, X, Y),C = (<).
% C = (<),
% X in 0..11,
% X#=<Y+ -1,
% zcompare(<, X, Y),
% Y in 11..20.

% ?- chain([A,B,C,D], #>=).
% A#>=B,
% B#>=C,
% C#>=D.

% ?- chain([5,B,C,4], #>=).
% B in 4..5,
% B#>=C,
% C in 4..5.

% ?- chain([5,B,4,D], #>=).
% B in 4..5,
% D in inf..4.

% ?- lex_chain([[1],[B],[C],[6]]).
% B in 1..6,
% C#>=B,
% lex_chain([[1], [B], [C], [6]]),
% freeze(B, clpfd:lex_le([1], [B])),
% freeze(B, clpfd:lex_le([B], [C])),
% C in 1..6,
% freeze(C, clpfd:lex_le([C], [6])).

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#133-elementindex-list-element
% Suzy Flirting Example
%
% Suzy wants to flirt with Nathan
% But not when her old boyfriend John is around
%
% Suzy, Nathan, and John all must take classes 1..6
%
% How can Suzy arrange her schedule so she can flirt
% in at least 3 classes?

flirt_constraint(Suzy, Nathan, John, FlirtPeriods) :-
	length(Suzy, 6),
	length(Nathan, 6),
	length(John, 6),
	Suzy ins 1..6,
	Nathan ins 1..6,
	John ins 1..6,
	all_different(Suzy),
	all_different(Nathan),
	all_different(John),
	FlirtPeriods = [A,B,C],
	FlirtPeriods ins 1..6,
	A #< B,    % remove unwanted symmetry
	B #< C,
	flirty_period(A, Suzy, Nathan, John),
	flirty_period(B, Suzy, Nathan, John),
	flirty_period(C, Suzy, Nathan, John),
	label(Suzy),
	label(FlirtPeriods).

flirty_period(Period, Suzy, Nathan, John) :-
	Class in 1..6,
	DiffClass #\= Class,
	element(Period, Suzy, Class),
	element(Period, Nathan, Class),
	element(Period, John, DiffClass).

% ?- flirt_constraint(Suzy, Nathan, John, FlirtPeriods).
% Suzy = [1, 2, 3, 4, 5, 6],
% Nathan = [1, 2, 3, _A, _B, _C],
% John = [_D, _E, _F, _G, _H, _I],
% FlirtPeriods = [1, 2, 3],
% _A in 4..6,
% all_different([1, 2, 3, _A, _B, _C]),
% _B in 4..6,
% _C in 4..6,
% _D in 2..6,
% all_different([_D, _E, _F, _G, _H, _I]),
% _E in 1\/3..6,
% _F in 1..2\/4..6,
% _G in 1..6,
% _H in 1..6,
% _I in 1..6 ;
% Suzy = [1, 2, 3, 4, 5, 6],
% Nathan = [1, 2, _A, 4, _B, _C],
% John = [_D, _E, _F, _G, _H, _I],
% FlirtPeriods = [1, 2, 4],
% _A in 3\/5..6,
% all_different([1, 2, _A, 4, _B, _C]),
% _B in 3\/5..6,
% _C in 3\/5..6,
% _D in 2..6,
% all_different([_D, _E, _F, _G, _H, _I]),
% _E in 1\/3..6,
% _F in 1..6,
% _G in 1..3\/5..6,
% _H in 1..6,
% _I in 1..6 ;
% Suzy = [1, 2, 3, 4, 5, 6],
% Nathan = [1, 2, _A, _B, 5, _C],
% John = [_D, _E, _F, _G, _H, _I],
% FlirtPeriods = [1, 2, 5],
% _A in 3..4\/6,
% all_different([1, 2, _A, _B, 5, _C]),
% _B in 3..4\/6,
% _C in 3..4\/6,
% _D in 2..6,
% all_different([_D, _E, _F, _G, _H, _I]),
% _E in 1\/3..6,
% _F in 1..6,
% _G in 1..6,
% _H in 1..4\/6,
% _I in 1..6 ;
% Suzy = [1, 2, 3, 4, 5, 6],
% Nathan = [1, 2, _A, _B, _C, 6],
% John = [_D, _E, _F, _G, _H, _I],
% FlirtPeriods = [1, 2, 6],
% _A in 3..5,
% all_different([1, 2, _A, _B, _C, 6]),
% _B in 3..5,
% _C in 3..5,
% _D in 2..6,
% all_different([_D, _E, _F, _G, _H, _I]),
% _E in 1\/3..6,
% _F in 1..6,
% _G in 1..6,
% _H in 1..6,
% _I in 1..5 ;
% Suzy = [1, 2, 3, 4, 5, 6],
% Nathan = [1, _A, 3, 4, _B, _C],
% John = [_D, _E, _F, _G, _H, _I],
% FlirtPeriods = [1, 3, 4],
% _A in 2\/5..6,
% all_different([1, _A, 3, 4, _B, _C]),
% _B in 2\/5..6,
% _C in 2\/5..6,
% _D in 2..6,
% all_different([_D, _E, _F, _G, _H, _I]),
% _E in 1..6,
% _F in 1..2\/4..6,
% _G in 1..3\/5..6,
% _H in 1..6,
% _I in 1..6 .
% .. and so on

% ?- circuit([A]).
% A = 1.

% ?- circuit([A,B]).
% A = 2,
% B = 1.

% ?- circuit([A,B,C]).
% A in 2..3,
% circuit([A, B, C]),
% B in 1\/3,
% C in 1..2.

% ?- circuit([A,B,C,D]).
% A in 2..4,
% circuit([A, B, C, D]),
% B in 1\/3..4,
% C in 1..2\/4,
% D in 1..3.

% ?- circuit([A,B,C,D,E]).
% A in 2..5,
% circuit([A, B, C, D, E]),
% B in 1\/3..5,
% C in 1..2\/4..5,
% D in 1..3\/5,
% E in 1..4.

% ?- length(Vs, _), circuit(Vs), label(Vs).
% Vs = [] ;
% Vs = [1] ;
% Vs = [2, 1] ;
% Vs = [2, 3, 1] ;
% Vs = [3, 1, 2] ;
% Vs = [2, 3, 4, 1] ;
% Vs = [2, 4, 1, 3] ;
% Vs = [3, 1, 4, 2] ;
% Vs = [3, 4, 2, 1] ;
% Vs = [4, 1, 2, 3] ;
% Vs = [4, 3, 1, 2] ;
% Vs = [2, 3, 4, 5, 1] ;
% Vs = [2, 3, 5, 1, 4] .
% ... and so on

binary_divisible_by_2(Bs) :-
  automaton(Bs, [source(a), sink(a)],
    [arc(a, 1, b),
     arc(b, 0, a),
     arc(b, 1, b),
     arc(a, 0, a)]).
    
% ?- binary_divisible_by_2([1,0]).
% true.
% 
% ?- binary_divisible_by_2([0,1]).
% false.
% 
% ?- binary_divisible_by_2([1,1]).
% false.
% 
% ?- binary_divisible_by_2([0]).
% true.
% 
% ?- binary_divisible_by_2([1]).
% false.
% 
% ?- binary_divisible_by_2([0,0,1,0]).
% true.
% 
% ?- binary_divisible_by_2([1,1,1,0]).
% true.

% ?- length(Bs, 4), binary_divisible_by_2(Bs), label(Bs).
% Bs = [0, 0, 0, 0] ;
% Bs = [0, 0, 1, 0] ;
% Bs = [0, 1, 0, 0] ;
% Bs = [0, 1, 1, 0] ;
% Bs = [1, 0, 0, 0] ;
% Bs = [1, 0, 1, 0] ;
% Bs = [1, 1, 0, 0] ;
% Bs = [1, 1, 1, 0].

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#14-scheduling
:- use_module(library(pairs)).

my_schedule_today(Starts, Durations) :-
  % unordered list of stuff to do today
  Starts = [PrepForSmith, MeetWithSmith, _WriteDBInstaller, Lunch, _CallAlice, _ReadDocs],
  % how long they'll take
  Durations = [2, 1, 2, 1, 1, 1],
  % hours in 24 hour format
  % 6am to 4pm
  Starts ins 6..16,
  % lunch at 11am or noon
  Lunch in 11 \/ 12,
  % meeting with Smith is at 1pm
  MeetWithSmith #= 13,
  % prep before the meeting
	PrepForSmith #< MeetWithSmith,
  serialized(Starts, Durations).

demo_my_schedule(Starts, Durations) :-
	my_schedule_today(Starts, Durations),
	append(Starts, Durations, Vars),
	label(Vars),
	pairs_keys_values(NameDurs ,
       ['Prep for Smith', 'Meet With Smith', 'Write DB Installer', 'Lunch', 'Call Alice', 'Read Flubbercalc Docs'], Durations),
	pairs_keys_values(Pairs, Starts, NameDurs),
	keysort(Pairs, Sorted),
	pairs_keys_values(Sorted, SortStarts, SortNameDurs),
	print_sched(SortStarts, SortNameDurs).

print_sched([], _).
print_sched([Start | ST], [Name-Duration | T]) :-
	format('~w: ~w  (~w hr)~n', [Start, Name, Duration]),
	print_sched(ST, T).

% ?- demo_my_schedule(Starts, Durations).
% 6: Prep for Smith  (2 hr)
% 8: Write DB Installer  (2 hr)
% 10: Call Alice  (1 hr)
% 11: Lunch  (1 hr)
% 12: Read Flubbercalc Docs  (1 hr)
% 13: Meet With Smith  (1 hr)
% Starts = [6, 13, 8, 11, 10, 12],
% Durations = [2, 1, 2, 1, 1, 1] ;
% 6: Prep for Smith  (2 hr)
% 8: Write DB Installer  (2 hr)
% 10: Call Alice  (1 hr)
% 11: Lunch  (1 hr)
% 13: Meet With Smith  (1 hr)
% 14: Read Flubbercalc Docs  (1 hr)
% Starts = [6, 13, 8, 11, 10, 14],
% Durations = [2, 1, 2, 1, 1, 1] ;
% 6: Prep for Smith  (2 hr)
% 8: Write DB Installer  (2 hr)
% 10: Call Alice  (1 hr)
% 11: Lunch  (1 hr)
% 13: Meet With Smith  (1 hr)
% 15: Read Flubbercalc Docs  (1 hr)
% Starts = [6, 13, 8, 11, 10, 15],
% Durations = [2, 1, 2, 1, 1, 1] ;
% 6: Prep for Smith  (2 hr)
% 8: Write DB Installer  (2 hr)
% 10: Call Alice  (1 hr)
% 11: Lunch  (1 hr)
% 13: Meet With Smith  (1 hr)
% 16: Read Flubbercalc Docs  (1 hr)
% Starts = [6, 13, 8, 11, 10, 16],
% Durations = [2, 1, 2, 1, 1, 1] ;
% 6: Prep for Smith  (2 hr)
% 8: Write DB Installer  (2 hr)
% 10: Read Flubbercalc Docs  (1 hr)
% 11: Lunch  (1 hr)
% 12: Call Alice  (1 hr)
% 13: Meet With Smith  (1 hr)
% Starts = [6, 13, 8, 11, 12, 10],
% Durations = [2, 1, 2, 1, 1, 1] .

% ?- [X,Y] ins 1..3, labeling([max(X), min(Y)], [X,Y]).
% X = 3,
% Y = 1 ;
% X = 3,
% Y = 2 ;
% X = Y, Y = 3 ;
% X = 2,
% Y = 1 ;
% X = Y, Y = 2 ;
% X = 2,
% Y = 3 ;
% X = Y, Y = 1 ;
% X = 1,
% Y = 2 ;
% X = 1,
% Y = 3 ;
% false.

% from https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc#17-debugging-and-testing
dom_integers(D, Is) :- phrase(dom_integers_(D), Is).

dom_integers_(I)      --> { integer(I) }, [I].
dom_integers_(L..U)   --> { numlist(L, U, Is) }, Is.
dom_integers_(D1\/D2) --> dom_integers_(D1), dom_integers_(D2).

% ?- X in 1..5, X #\= 4, fd_dom(X, D), dom_integers(D, Is).
% D = 1..3\/5,
% Is = [1, 2, 3, 5],
% X in 1..3\/5.

% ?- X in 0..10, Y in 0..5, X #< Y, copy_term(foo(X,Y), foo(XA, YA)), YA = 3.
% YA = 3,
% X in 0..4,
% X#=<Y+ -1,
% Y in 1..5,
% XA in 0..2.

% ?- X in 0..10, Y in 0..5, X #< Y, copy_term(foo(X,Y), foo(XA, YA)), XA = 3.
% XA = 3,
% X in 0..4,
% X#=<Y+ -1,
% Y in 1..5,
% YA in 4..5.

% ?- X in 0..10, Y in 0..5, X #< Y, copy_term([Y], [YA]), YA = 3.
% YA = 3,
% X in 0..4,
% X#=<Y+ -1,
% Y in 1..5.

% ?- X in 0..10, Y in 0..5, X #< Y, copy_term([Y], [YA]), X = 3.
% X = 3,
% Y in 4..5,
% YA in 1..5,
% _A#=<YA+ -1,
% _A in 0..4.

% ?- X in 1..3, Y #= X + 2, copy_term([X], [X1]), Y #= 3.
% X = 1,
% Y = 3,
% X1 in 1..3,
% X1+2#=_A,
% _A in 3..5.

% ?- X in 1..3, Y #= X + 2, copy_term([X], [X1]), Y #= 3, X1 == 1.
% false.