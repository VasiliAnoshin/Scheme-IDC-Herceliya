/* Set of different questions of prolog/*

start_with(S1, S2):- append(S1,_,S2).
start_with2([],_).
start_with2([H|L],[H|L1]):- start_with2(L,L1).

ends_with2(S,S).
ends_with2(S,[X|L1]):- ends_with2(S,L1).
ends_with2([],_).
ends_with(S,S2):- append(_, S, S2).

longer([],[R],[R]).
longer([R],[],[R]).
longer([H|L],[_|L1],[H|L]):- longer(L,L1,L).
longer([_|L],[H1|L1],[H1|L1]):- longer(L,L1,L1).

sum_and_mult([],0).
sum_and_mult([[H|L]|L2],R):-sum_and_mult(L,R2), X1 is H * R2, sum_and_mult(L2,R3), R is R3 + X1,!.
sum_and_mult([H|L],R):- sum_and_mult(L,R2), R is H + R2.

set_minus([],_, []).
set_minus([H|L] , L2 , R):- member(H,L2), set_minus(L, L2, R), !.
set_minus([H|L] , L2 , [H|R]):- set_minus(L,L2,R).

subset([_|S],S).
subset([A|S],[A|S1]):- subset(S,S1).
subset([_|S],S1):- subset(S,S1).

print_sub_sets1(S):- subset(S,X), write(X), write(','),fail.
print_sub_sets1(_).

my_reverse([],[]).
my_reverse([X|L],R):- my_reverse(L,R1), append([X], R1, R).
			    
contains(S,S).
contains([H|L1], [H|L2]):- append([H|L2],_,[H|L1]), !.
contains([_|L], L2):- contains(L,L2).

flatten1([], []).
flatten1([[H|L]|L2] , R):- flatten1(L, R1), flatten1(L2,R2), append([H],R1,RES), append(RES,R2,R),!.
flatten1([H|L],R):- flatten1(L,R1), append([H],R1,R).
flatten1([X],[X]).

flatten2([],[]).
flatten2([[X|L1]|L],R):- flatten2(X,XF), flatten2(L,LF), flatten2(L1,L1F),append(XF,L1F,XR),append(XR,LF,R), !.
flatten2([X|L], [X|R]):- flatten2(L,R), !.
flatten2(X,[X]).

find_min([X], X).
find_min([H1,H2|L], R):- H1>=H2, append([H2],L,RES1), find_min(RES1,R),!.
find_min([H1,H2|L],R):- H1 < H2, append([H1],L,RES1), find_min(RES1,R),!.

find_min2([X], X).
find_min2([X|Y],X):- find_min2(Y,RES), X < RES,!.
find_min2([_|Y],X):- find_min2(Y,X), !.

take_min(S,X,R):- find_min(S,X), take_minimum(S,X,R).
take_minimum([H|L],X,[H|RES]):- X=\=H, take_minimum(L,X,RES),!.
take_minimum([H|L],X,RES):- X=:=H, take_minimum(L,X,RES).
take_minimum([], _ , []). 

find_k(S,0,R):-find_min(S,R), !.
find_k(S,X,R):- X1 is X - 1, find_min(S,X2),take_min(S,X2,RES), find_k(RES,X1,R), !.

selection_sort([],[]).
selection_sort(S,R):- take_min(S,MIN,RES),selection_sort(RES,R2), append([MIN],R2,R).

issorted([]).
issorted([_]).
issorted([H1,H2|L]):- H1=<H2, append([H2],L,RES),issorted(RES). 

countlevel([],1).
countlevel([[H|L]|L1],N):- countlevel([H|L],N1),countlevel(L1,N2), N is N1+N2.
countlevel([_|L],N):- countlevel(L,S1), N is S1, !.
countlevel([_],1).

put_it_end(X1,S,X):-append(S,[X1],X),!.
without_last(S,S1):- put_it_end(_,S,S1),!. 