camino(E,E,C,C).
camino(EstadoActual,EstadoFinal, CaminoHastaAhora, CaminoTotal):-
    unPaso(EstadoActual,EstSiguiente),
    not(member(EstSiguiente,CaminoHastaAhora)),
    camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],CaminoTotal).




possible(C,N):- C =< N, C >= 1.

% C és la coordenada, que pot ser tant X com Y
operation(C1, C, C2):- C2 is C1 + C.
operation(C1, C, C2):- C2 is C1 - C.

unPaso([(X1,Y1),N,P1,PasosMaxims],[(X2,Y2),N,P2,PasosMaxims]):-
    between(1,2,X), between(1,2,Y),
    P2 is P1 + 1, P2 =< PasosMaxims,
    T is X + Y, T == 3,
    operation(X1,X,X2), operation(Y1,Y,Y2),
    possible(X2, N), possible(Y2, N).
    %write("X1: "), write(X1), write(" Y1: "), write(Y1),
    %write(" X: "), write(X), write(" Y: "), write(Y),
    %write(" X2: "), write(X2), write(" Y2: "), write(Y2), nl.


nat(8).
%nat(S):- nat(N), S is N+1.

% Pasos de caball
numPasos(0).
numPasos(P):- numPasos(P1), P is P1 + 1.


writeList([]):- !.
writeList([X|C]):- writeList(C), nl, write(X).

solucionOptima:-
    nat(N),
    numPasos(P),
    camino([(1,1),N,0,P],[(8,8),N,_,P],[[(1,1),N,0,P]],C),
    length(C,P),
    write(P),
    writeList(C), nl, halt.


main:-solucionOptima.
