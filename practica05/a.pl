concat([],L,L).
concat([X|L1],L2,[X|L3]):- concat(L1,L2,L3).

pert_con_resto(X,L,Resto):- concat(L1,[X|L2],L), concat(L1,L2,Resto).

permutacion([],[]).
permutacion(L,[X|P]) :- pert_con_resto(X,L,R), permutacion(R,P).


subcjto([],[]).  %subcjto(L,S) significa "S es un subconjunto de L".
subcjto([X|C],[X|S]):-subcjto(C,S).
subcjto([_|C],S):-subcjto(C,S).

cifras(L,N):- length(L,LEN), between(1,LEN,R), subcjto(L,S), length(S,LENS), 
    LENS == R, permutacion(S,P), expresion(P,E), 
    N is E, write(E),nl,fail,halt.  
              
              
expresion([X],X).                                             
expresion(L,E1+E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      
expresion(L,E1-E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).      
expresion(L,E1*E2):- concat(L1,L2,L),  L1\=[],L2\=[],         
                     expresion(L1,E1), expresion(L2,E2).   
expresion(L,E1/E2):- concat(L1,L2,L), L1\=[],L2\=[],         
                     expresion(L1,E1), E2 \= 0,expresion(L2,E2). 

main:- cifras([4,9,8,7,100,4],380).
