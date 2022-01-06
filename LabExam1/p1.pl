swap([X,Y],[X,Y]).
swap([X,Y],[Y,X]).

permutacion([],[]).
permutacion(L,[Y|P]):- select(X,L,R), swap(X,Y), permutacion(R,P).

basicPerm([],[]).
basicPerm([X|L],[Y|R]):- swap(X,Y), basicPerm(L,R).

isChain([],_).
isChain([[X,Y]|L],[X,Y]):- isChain(L,[Y,_]).


chain(L,R):- basicPerm(L,R), isChain(R,_).


perm([],[]).
perm([_|L1],L2):- perm(L1,L2).
perm([X|L1],[X|L2]):- perm(L1,L2).

all_chains(L,R):- perm(L,S), permutation(S,P), chain(P,R).


%main:- all_chains([[1,2],[3,2],[3,1]],R), write(R), nl, fail.


path(_,S,S):- !.
path(L,S,F):- select([S,Y],L,L2), path(L2,Y,F),!.


main:- path([[1,2],[2,3],[3,1],[1,3],[3,4],[4,5],[5,4],[4,6]],3,2), write("HELL YES"), nl, fail.





































