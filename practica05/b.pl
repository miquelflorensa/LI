camino(E,E,C,C).
camino(EstadoActual,EstadoFinal, CaminoHastaAhora, CaminoTotal):-
    unPaso(EstadoActual,EstSiguiente),
    not(member(EstSiguiente,CaminoHastaAhora)),
    camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],CaminoTotal).


% ML1 -> Misioner Left abans
% ML2 -> Misioner Left després

% CR1 -> Caníbal Right abans
% CR2 -> Caníbal Right després

% etc ...

possible(0,_).
possible(M,C):- M >= C.

unPaso([(ML1,CL1),(MR1,CR1),0],[(ML2,CL2),(MR2,CR2),1]):-
    between(0,2,M), M =< ML1, between(0,2,C), C =< CL1,
    T is M + C, between(1,2,T),
    ML2 is ML1 - M, CL2 is CL1 - C,
    MR2 is MR1 + M, CR2 is CR1 + C,
    possible(ML2,CL2),
    possible(MR2,CR2).

unPaso([(ML1,CL1),(MR1,CR1),1],[(ML2,CL2),(MR2,CR2),0]):-
    between(0,2,M), M =< MR1, between(0,2,C), C =< CR1,
    T is M + C, between(1,2,T),
    MR2 is MR1 - M, CR2 is CR1 - C,
    ML2 is ML1 + M, CL2 is CL1 + C,
    possible(ML2,CL2),
    possible(MR2,CR2).

nat(0).
nat(S):- nat(N), S is N+1.


writeList([]):- !.
writeList([X|C]):- writeList(C), nl, write(X).

solucionOptima:-
    nat(N),
    camino([(3,3),(0,0),0],[(0,0),(3,3),1],[[(3,3),(0,0),0]],C),
    length(C,N),
    write(N),
    writeList(C),nl,halt.


main:-solucionOptima.
