path(_,S,S):- !.
path(L,S,F):- select([S,Y],L,L2), path(L2,Y,F),!.


%main:- path([[1,2],[2,3],[3,1],[1,3],[3,4],[4,5],[5,4],[4,6]],3,2), write("HELL YES"), nl, fail.



negate(\+X,X):-!.

negate(X,\+X).

sat(N,S):-

   findall([NotY,X] ,(member([X,Y],S), negate(Y,NotY)),G1),

   findall([NotX,Y] ,(member([X,Y],S), negate(X,NotX)),G2), append(G1,G2,G),

   \+badCycle(N,G).

   

badCycle(N,G):- between(1,N,S), negate(S,NS), path(G,S,NS), path(G,NS,S).


main:-sat(6,[[\+1,2],[\+2,3],[\+4,\+6],[\+2,\+4],[4,5],[\+6,1],[\+5,\+2],[2,1]]), write("HOLLY COW"), nl, fail, halt.