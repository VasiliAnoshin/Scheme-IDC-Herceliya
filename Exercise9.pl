/* 
* Exercise 9 321464935
* Part 1 
* 1: Binding , X = p1(a b)
* 2: Binding , X = Y = 36
* 3: Fail   , b will not match x
* 4: Binding , X=B=p2(9)
* 5: Fail   , It can't be that A = 4 && A=3 
* 6: Binding , A = B = 3 , C = p3(B)
* 7: Fail   , It can't be that Y equal symbol a and equal 3.    
* 8: Binding , X=p3(Z,8) , Z = 9, p2(2,X) = W 
* 9: Binding , A=p2(A, X), B = 2, a = X , Y = 3    
* 

*Part 2 A
*Number is Odd :
*/
numberIsOdd(0) :- fail.
numberIsOdd(1) :- !.
numberIsOdd(X) :- X > 0, X1 is X-2, numberIsOdd(X1).

/*
 * Part 2 B 
 * x is the element index
 * y is the value of that element
----------------------------------------------------------------------------------
 
First in X1 i reduce index of iteration by one. Calling the recursion once again until i get (1,_X). 
Then i get the new value of R that consist from current index X and X2 that i recieve from stop condition.
Finish the iteration and return to previous iteration. We continue do these until go back to start of the recursion calling . 
And then we will check the user unput, what is R. If R equal R - we return the operation result. If R equal some number we check if these
number is equal to result .     
*/

triangularNth(1,1) :- !.
triangularNth(X, R) :- X > 0, X1 is X - 1, triangularNth(X1,X2), R is X + X2.



/*
Part 2 C:
FINDMIN
----------------------------------------

	If  User enter "R" - return minValue
*/
find_minimum([] ,MinValue, UserValue):- UserValue = R, R is MinValue, !.
/*
	If User enter some minimal Value , check if this value equal to real minimal value . If yes Return true.  
*/
find_minimum([] ,MinValue, UserValue):- MinValue =:= UserValue, !.
/*
* Check three possible options for First Value of the list , if  biger , equal or smaller than exist minValue. Continue iterate until reach empty list. 
*/
find_minimum([H|T] , MinValue ,UserValue) :- H \= [],  H < MinValue,   find_minimum(T, H,  UserValue).
find_minimum([H|T] , MinValue ,UserValue) :- H \= [],  H =:= MinValue, find_minimum(T, H, UserValue).
find_minimum([H|T] , MinValue ,UserValue) :- H \= [],  >(H, MinValue), find_minimum(T, MinValue, UserValue).
/*
* This is a start of the program !!!!!!!!!!!
* Take the maximal number be the MinValue , thats for taking the first element of the list be the min value in first iteration of the help function.
*/
find_min([H|T] , UserValue) :- MinValue is 1000000000000, find_minimum([H|T] , MinValue , UserValue),!.