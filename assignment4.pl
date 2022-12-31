/* YOUR CODE HERE (Problem 1, delete the following line) */
range(S,E,M) :- M>=S,M=<E.

?- range(1,2,2).
?- not(range(1,2,3)).

/* YOUR CODE HERE (Problem 2, delete the following line) */
reverseL([],[]).
reverseL([H|P],RevX):- reverseL(P,RevP),append(RevP,[H],RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

/* YOUR CODE HERE (Problem 3, delete the following line) */
memberL(_,[]):-false.
memberL(X,[H|T]):- X is H;memberL(X,T).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

/* YOUR CODE HERE (Problem 4, delete the following line) */
zip([], _, []).
zip(_,[],[]).
zip([H1|P1],[H2|P2],Z):-append([H1-H2],Rest,Z),zip(P1,P2,Rest).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

/* YOUR CODE HERE (Problem 5, delete the following line) */
insert(X, [], [X]).
insert(X,[H|T],[X,H|T]):-X=<H.
insert(X,[H|T],[H|R]):-X>H,insert(X,T,R).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

/* YOUR CODE HERE (Problem 6, delete the following line) */
remove_duplicates(L1,L2):-removedup_helper(L1,[],L2).
removedup_helper([],_,[]).
removedup_helper([H|T],Acc,L2):-member(H,Acc),removedup_helper(T,Acc,L2).
removedup_helper([H|T],Acc,[H|L2]):-not(member(H,Acc)),removedup_helper(T,[H|Acc],L2).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

/* YOUR CODE HERE (Problem 7, delete the following line) */
intersectionL([],_,[]).
intersectionL([H|T],L2,[H|R]):-member(H,L2),intersectionL(T,L2,R).
intersectionL([H|T],L2,R):-not(member(H,L2)),intersectionL(T,L2,R).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

/* YOUR CODE HERE (Problem 8, delete the following line) */
partition([],[],[]).
partition([H],[H],[]). 
partition(L,P,S):-prefix(P,L),suffix(S,L),length(L,LL),PL is div(LL,2),
    SL is LL-PL,length(P,PL),length(S,SL).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

/* YOUR CODE HERE (Problem 9, delete the following line) */
merge(L1,[],L1).
merge([],L2,L2).
merge([H1|T1],[H2|T2],Z):-H1=<H2,append([H1],R,Z),merge(T1,[H2|T2],R).
merge([H1|T1],[H2|T2],Z):-H2<H1,append([H2],R,Z),merge([H1|T1],T2,R).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

/* YOUR CODE HERE (Problem 10, delete the following line) */
mergesort([],[]).
mergesort([X],[X]).
mergesort(L,R):-L=[_,_|_],partition(L,P,S),mergesort(P,R1),mergesort(S,R2),merge(R1,R2,R).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).