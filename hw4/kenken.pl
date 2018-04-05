% 1 Kenken

% Gets the square in T at [Row|Col]
get_square(Row, Col, T, Square) :-
  nth(Row, T, R), nth(Col, R, Square).

% Check if the values in Coords add up to Sum
sum(0, [], T).
sum(Sum, [[Row|Col] | Coords], T) :-
  get_square(Row, Col, T, Square),
  Square + Remainder #= Sum,
  sum(Remainder, Coords, T).

% Check if the values in Coords multiply up to Prod
prod(1, [], T).
prod(Prod, [[Row|Col] | Coords], T) :-
  get_square(Row, Col, T, Square),
  Square * Remainder #= Prod,
  prod(Remainder, Coords, T).

% Check if the two Coords' difference is Diff
diff(Diff, [Row1|Col1], [Row2|Col2], T) :-
  get_square(Row1, Col1, T, Square1),
  get_square(Row2, Col2, T, Square2),
  (Square1 - Square2 #= Diff; Square2 - Square1 #= Diff).

% Check if the two Coords' quotient is Quotient
quot(Quot, [Row1|Col1], [Row2|Col2], T) :-
  get_square(Row1, Col1, T, Square1),
  get_square(Row2, Col2, T, Square2),
  (Square1 / Square2 #= Quot; Square2 / Square1 #= Quot).

% Check if all cage constraints are met with T
meets_cage_constraints([], T).
meets_cage_constraints([+(Sum, Coords) | Cs], T) :-
  sum(Sum, Coords, T),
  meets_cage_constraints(Cs, T).
meets_cage_constraints([-(Diff, Coord1, Coord2) | Cs], T) :-
  diff(Diff, Coord1, Coord2, T),
  meets_cage_constraints(Cs, T).
meets_cage_constraints([*(Prod, Coords) | Cs], T) :-
  prod(Prod, Coords, T),
  meets_cage_constraints(Cs, T).
meets_cage_constraints([/(Quot, Coord1, Coord2) | Cs], T) :-
  quot(Quot, Coord1, Coord2, T),
  meets_cage_constraints(Cs, T).

% Transpose, retrieve the columns of matrix
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% Check if every row in T contains values within the domain, all unique.
rows_meet_constraints(N, []).
rows_meet_constraints(N, [H | T]) :-
  length(H, N), fd_domain(H, 1, N), fd_all_different(H), rows_meet_constraints(N, T).

% Check if the rows and cols in T match the constraints in rows_meet_constraints
meets_constraints(N, T) :-
  length(T, N), 
  rows_meet_constraints(N, T),
  transpose(T, Cols),
  rows_meet_constraints(N, Cols).

% N, a nonnegative integer specifying the number of cells on each side of the KenKen square.
% C, a list of numeric cage constraints as described below.
% T, a list of list of integers. All the lists have length N. This represents the N×N grid.
kenken(N, C, T) :-
  meets_constraints(N, T),
  meets_cage_constraints(C, T),
  maplist(fd_labeling, T).

% 2 plain kenken

% implementation of kenken without using finite domain solver

plain_sum(0, [], T).
plain_sum(Sum, [[Row|Col] | Coords], T) :-
  get_square(Row, Col, T, Square),
  Remainder is (Sum - Square),
  plain_sum(Remainder, Coords, T).

plain_prod(1, [], T).
plain_prod(Prod, [[Row|Col] | Coords], T) :-
  get_square(Row, Col, T, Square),  
  Remainder is (Sum / Square),
  plain_prod(Remainder, Coords, T).

plain_diff(Diff, [Row1|Col1], [Row2|Col2], T) :-
  get_square(Row1, Col1, T, Square1),
  get_square(Row2, Col2, T, Square2),
  (Diff is Square1 - Square2; Diff is Square2 - Square1).

plain_quot(Quot, [Row1|Col1], [Row2|Col2], T) :-
  get_square(Row1, Col1, T, Square1),
  get_square(Row2, Col2, T, Square2),
  (Square1 is Quot * Square2; Square2 is Quot * Square1).

plain_meets_cage_constraints([], T).
plain_meets_cage_constraints([+(Sum, Coords) | Cs], T) :-
  plain_sum(Sum, Coords, T),
  plain_meets_cage_constraints(Cs, T).
plain_meets_cage_constraints([-(Diff, Coord1, Coord2) | Cs], T) :-
  plain_diff(Diff, Coord1, Coord2, T),
  plain_meets_cage_constraints(Cs, T).
plain_meets_cage_constraints([*(Prod, Coords) | Cs], T) :-
  plain_prod(Prod, Coords, T),
  plain_meets_cage_constraints(Cs, T).
plain_meets_cage_constraints([/(Quot, Coord1, Coord2) | Cs], T) :-
  plain_quot(Quot, Coord1, Coord2, T),
  plain_meets_cage_constraints(Cs, T).

plain_rows_meet_constraints(N, []).
plain_rows_meet_constraints(N, [H | T]) :-
  length(H, N), 
  findall(Num, between(1, N, Num), Domain), permutation(H, Domain),
  plain_rows_meet_constraints(N, T).

plain_meets_constraints(N, T) :-
  length(T, N), 
  plain_rows_meet_constraints(N, T),
  transpose(T, Cols),
  plain_rows_meet_constraints(N, Cols).

plain_kenken(N, C, T) :-
  plain_meets_constraints(N, T),
  plain_meets_cage_constraints(C, T).

% Performance (tested on lnxsrv07, using kenken_testcase2)
%
% I used the commands,
%   fd_set_vector_max(255), kenken_testcase2(N,C), kenken(N,C,T), write(T), nl, fail.
%   kenken_testcase2(N,C), plain_kenken(N,C,T), write(T), nl, fail.
% then used statistics/0 to record the stats after the command.
% The results were:
%
%   kenken
%   ======
%   Times              since start      since last
%
%    user   time       0.007 sec       0.007 sec
%    system time       0.004 sec       0.004 sec
%    cpu    time       0.011 sec       0.011 sec
%    real   time      14.029 sec      14.029 sec
%
%   plain_kenken
%   ============
%   Times              since start      since last
%
%    user   time       4.845 sec       4.845 sec
%    system time       0.003 sec       0.003 sec
%    cpu    time       4.848 sec       4.848 sec
%    real   time      14.100 sec      14.100 sec

% 3 no-op kenken

% Checks if the values in the Coords evaluates to Target,
% based on the last parameter, an integer, that indicates,
% the operation in the evaluation:
%   add = 1, subtract = 2, multiply = 3, divide = 4.
which_operation(Target, [], T, Num).
which_operation(Target, [[Row|Col] | Coords], T, 1).
which_operation(Target, [[Row|Col] | Coords], T, 2).
which_operation(Target, [[Row|Col] | Coords], T, 3).
which_operation(Target, [[Row|Col] | Coords], T, 4).

% First calls which_operation with the call
%   which_operation(Target, Coords, T, Num),
% and based on what Num is, call the appropriate function
% (sum, diff, prod, or quot)
meets_cage_constraints([(Target, Coords) | Cs], T).

% The top-level call is the same as the other two 
% implementations of Kenken.
noop_kenken(N, C, T).

% Test cases
kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

kenken_testcase2(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).

noop_kenken_testcase(
  4,
  [
    (12, [[1|1], [1|2], [1|3]]),
    (12, [[2|1], [3|1]]),
    (2, [[2|2], [2|3]]),
    (16, [[4|1], [4|2], [3|2]]),
    (9, [[3|3], [4|3], [4|4]]),
    (8, [[1|4], [2|4], [3|4]])
  ]
).

% With this test case,

% noop_kenken_testcase(
%   4,
%   [
%     (12, [[1|1], [1|2], [1|3]]),
%     (12, [[2|1], [3|1]]),
%     (2, [[2|2], [2|3]]),
%     (16, [[4|1], [4|2], [3|2]]),
%     (9, [[3|3], [4|3], [4|4]]),
%     (8, [[1|4], [2|4], [3|4])
%   ]
% ), write(T), nl, fail.

% it should output (reindented to fit):

% [[1,3,4,2],[3,1,2,4],[4,2,3,1],[2,4,1,3]].
