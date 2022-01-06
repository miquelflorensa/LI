camino(E,E,C,C).
camino(EstadoActual,EstadoFinal, CaminoHastaAhora, CaminoTotal):-
    unPaso(EstadoActual,EstSiguiente),
    %write(EstadoActual), nl,
    not(member(EstSiguiente,CaminoHastaAhora)),
    camino(EstSiguiente,EstadoFinal,[EstSiguiente|CaminoHastaAhora],CaminoTotal).

subset([], []).
subset([E|Tail], [E|NTail]):-
    subset(Tail, NTail).
subset([_|Tail], NTail):-
    subset(Tail, NTail).

max([],0).
max([E|Tail],M):- max(Tail,C), M is E + C.

eliminaSubset(PL1,[],PL1).
eliminaSubset(PL1,[X|SL1],R):- eliminaSubset(PL1,SL1,R1), select(X,R1,R).

llistaOrdenada([X],X).
llistaOrdenada([X|L],X):- llistaOrdenada(L,Y), Y >= X.

ordenaPersonas(L1,L2):- permutation(L2,L1), llistaOrdenada(L2,_).

unPaso([PL1,PR1,0,CostAbans,CostMaxim],[PL2,PR2,1,CostDespres,CostMaxim]):-
    subset(PL1,SL1), length(SL1,L), between(1,2,L),
    max(SL1,M), CostDespres is CostAbans + M, CostDespres =< CostMaxim,
    eliminaSubset(PL1,SL1,PL2), append(PR1,SL1,P2),
    ordenaPersonas(P2,PR2). % write(PR2), nl.

unPaso([PL1,PR1,1,CostAbans,CostMaxim],[PL2,PR2,0,CostDespres,CostMaxim]):-
    subset(PR1,SR1), length(SR1,L), between(1,2,L),
    max(SR1,M), CostDespres is CostAbans + M, CostDespres =< CostMaxim,
    eliminaSubset(PR1,SR1,PR2), append(PL1,SR1,P2),
    ordenaPersonas(P2,PL2). % write(PL2), nl.

nat(0).
nat(S):- nat(N), S is N+1.


writeList([]):- !.
writeList([X|C]):- writeList(C), nl, write(X).

personas([1,2,5,8]).



solucionOptima:-

    nat(N),
    personas(P),
    camino([P,[],0,0,N],[[],P,1,Cost,N],[[P,[],0,0,N]],C),
    N == Cost,
    write(N),
    writeList(C),nl,halt.


main:-solucionOptima.
