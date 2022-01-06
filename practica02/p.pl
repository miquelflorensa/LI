%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%      Practica 2 Miquel Florensa Grup 21       %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------
% -1-

prod([X],X).
prod([X|L],Y):- prod(L,R), Y is X * R.

%main:- prod([1,2,3],X),write(X),nl,fail,halt.


% -------------------------------------------------------------------
% -2-

pescalar([],[],0).
pescalar([X|L1],[Y|L2],P):- pescalar(L1,L2,R), P is (X * Y) + R.

%main:- pescalar([1,2,3],[1,2,3],X),write(X),nl,fail,halt.

% -------------------------------------------------------------------
% -3-

inter([],_,[]).
inter([X|L1],L2,[X|L]):- member(X,L2),!, inter(L1,L2,L).
inter([X|L1],L2,L):- (member(X,L2)), inter(L1,L2,L).

%main:- inter([1,2,3],[1,2,3],X),write(X),nl,halt.

unio(L1,[],L1).
unio(L1,[X|L2],L):- member(X,L1), unio(L1,L2,L).
unio(L1,[X|L2],[X|L]):- unio(L1,L2,L).

%main:- unio([1,2,3,4],[1,2,3,5,7],X),write(X),nl,halt.


% -------------------------------------------------------------------
% -4-

ultim([X],X).
ultim([_|L],U):- ultim(L,U).

%main:- ultim([1,2,3,4,5],X),write(X),nl,halt.

invers([X],[X]).
invers([X|T],L):- invers(T,I), append(I,[X],L).

%main:- invers([1,2,3,4,5],X),write(X),nl,halt.

% -------------------------------------------------------------------
% -5-

fib(0,0).
fib(1,1).
fib(N,F):- N > 1, N1 is N - 1, N2 is N - 2, fib(N1,F1), fib(N2, F2), F is F1 + F2.

% main:- fib(7,X),write(X),nl,fail,halt.

% -------------------------------------------------------------------
% -6-

dados(0,0,[]).
dados(P,N,[X|L]):- N > 0, member(X,[1,2,3,4,5,6]), P1 is P-X, N1 is N-1, dados(P1,N1,L).

%main:- dados(7,2,X),write(X),nl,fail,halt.

% -------------------------------------------------------------------
% -7-

suma([],0).
suma([X|L],R):- suma(L,R2), R is X + R2.

suma_demas([],0).
suma_demas([X|L],R):- suma(L,Y), Y == X, R is X,!.
suma_demas([_|L],R):- suma_demas(L,R).

%main:- suma_demas([18,9,8,1],T),write(T),nl,fail,halt.

% -------------------------------------------------------------------
% -8-

suma_ants(L):- append(L1,[X|_],L), suma(L1,X), write("True"),!.

%main:- suma_ants([1,8,9,18]),nl,fail,halt.

% -------------------------------------------------------------------
% -9-

cardinalidad([],_,0).
cardinalidad([Y|L],Y,V):- cardinalidad(L,Y,V2), V is V2+1.
cardinalidad([X|L],Y,V):- dif(X,Y), cardinalidad(L,Y,V).

elimina([],_,[]).
elimina([X|L],X,L2):- elimina(L,X,L2),!.
elimina([X|L],Y,L2):- elimina(L,Y,L3), append([X],L3,L2).

card([],[]).
card([X|L],[[X,L2]|L3]):- cardinalidad(L,X,R), L2 is R+1, elimina(L,X,EL), card(EL,L3).

%main:- card([1,2,1,5,1,3,3,7], X), write(X),nl,fail,halt.

% -------------------------------------------------------------------
% -10-

esta_ordenada([X],X).
esta_ordenada([X|L],X):- esta_ordenada(L,Y), Y =< X.

%main:-esta_ordenada([3,45,67,3],_),write("yes"),nl,fail,halt.

% -------------------------------------------------------------------
% -11-

ord(L1,L2):- permutation(L2,L1), esta_ordenada(L2,_),!.

%main:-ord([4,5,3,3,2],X),write(X),nl,fail,halt.

% -------------------------------------------------------------------
% -12-

dicc(_,0,[]):-!.
dicc(A,N,[P|R]):-member(P,A),N1 is N-1,dicc(A,N1,R).

diccionario(A,N):- dicc(A,N,R),write(R),write(" "),fail.

%main:-diccionario([ga,chu,le],5),halt.

% -------------------------------------------------------------------
% -13-

palindromos(L):-setof(A, (permutation(L,A),invers(A,I), A == I), A),write(A).

%main:-palindromos([a,a,c,c]),nl,fail,halt.

% -------------------------------------------------------------------
% -14-

money:-permutation([S,E,N,D,M,O,R,Y,_,_],[0,1,2,3,4,5,6,7,8,9]),
S1 is (D + N*10 + E*100 + S*1000) + (E + R*10 + O*100 + M*1000),
S1 is (Y + E*10 + N*100 + O*1000 + M*10000),
write(M), write(" "),
write(O), write(" "),
write(N), write(" "),
write(E), write(" "),
write(Y), write(" "),!,nl,fail,halt.

%main:-money(),nl,fail,halt.

% -------------------------------------------------------------------
% -15-

sim(A,B):- unpas(A,C),!, sim(C,B).
sim(A,A):-!.

unpas(A,B):- pasSuperior(A,B).
unpas(A,B):- pasSubExpressio(A,B).

pasSubExpressio(A,B):-
    A=..[F|La],
    append(L1,[Ea|L2],La),
    unpas(Ea,Eb),
    append(L1,[Eb|L2],Lb),
    B=..[F|Lb].

pasSuperior(A+0,A).
pasSuperior(0+B,B).
pasSuperior(A-0,A).
pasSuperior(0-B,-B).
pasSuperior(A-A,0).
pasSuperior(A*1,A).
pasSuperior(1*B,B).
pasSuperior(A/1,A).
pasSuperior(_*0,0).
pasSuperior(0*_,0).
pasSuperior(0/_,0).
pasSuperior(A+B,N ):- integer(A), integer(B), N is A+B.
pasSuperior(A*B,N ):- integer(A), integer(B), N is A*B.
pasSuperior(A-B,N ):- integer(A), integer(B), N is A-B.
pasSuperior(A//B,N):- integer(A), integer(B), B\=0, N is A//B.



% -------------------------------------------------------------------
% -16-

ok([X],X).
ok([f(X1,X2)|L],f(X1,X2)):- ok(L,f(Y1,_)),Y1 == X2.

swap(f(X,Y),f(X,Y)).
swap(f(X,Y),f(Y,X)).

p([],[]).
p(L,[Y|P]) :- select(X,L,R), swap(X,Y) , p(R,P).

dom(L) :- p(L,P), ok(P,_), write(P), nl.
dom(_) :- write("no hay cadena"), nl.

%main:-dom([f(3,4),f(2,3),f(1,6),f(2,2),f(4,2),f(2,1)]),fail,halt.


% -------------------------------------------------------------------
% -17-

decision_lit([[Lit|_]|_],Lit).
decision_lit(F,Lit):- member([Lit],F), !.

simplif(_,[],[]).
simplif(Lit,F,F1):- 
	member(C,F), LitN is Lit * (-1), 
	member(LitN,C),!, select(C,F,F2), 
	select(LitN,C,C2), length(C2, L2), L2 > 0,
	append([C2],F2,F3), simplif(Lit,F3,F1).
simplif(Lit,F,F1):- 
	member(C,F), member(Lit,C),!,
	select(C,F,F2), simplif(Lit,F2,F1).
simplif(_,F,F).

sat(I,[]):- write('IT IS SATISFIABLE. Model: '), write(I),nl,!.
sat(I,F):-
    decision_lit(F,Lit), % Select unit clause if any; otherwise, an arbitrary one.
    simplif(Lit,F,F1), % Simplifies F. Warning: may fail and cause backtracking
    sat([Lit|I],F1).

p(F):- sat([],F).
p(_):- write('UNSAT'),nl.

main:- p([[1,2],[1,-2],[-1,-2],[-1,-2]]),halt.


% -------------------------------------------------------------------
% -18-

percentages():- 
	between(1,10,X), between(1,X,XSC), 
	XNS is 10 - X, between(1,XNS,XNSC), 
	XTSC is X * XSC, XTNSC is XNS * XNSC, XTSC > XTNSC,

	between(1,10,Y), between(1,Y,YSC), 
	YNS is 10 - Y, between(1,YNS,YNSC), 
	YTSC is Y * YSC, YTNSC is YNS * YNSC, YTSC > YTNSC,

	F is X+Y, FSC is XSC + YSC,
	FNS is 20 - F, FNSC is XNSC + YNSC,
	FTSC is F * FSC, FTNSC is FNS * FNSC, FTSC < FTNSC,

	write(X-XSC-XNSC), nl,
	write(Y-YSC-YNSC).

%main:-percentages(), nl, fail, halt.


% -------------------------------------------------------------------
% -19-

cardi(_,[],0).
cardi(X,[X|L],Z):- cardi(X,L,Z2), Z is Z2 +1.
cardi(X,[_|L],Z):- cardi(X,L,Z).  

convertModel(_,[],[]).
convertModel(C,[X|L],[Y|L1]):- cardi(X,C,Y),!, convertModel(C,L,L1).

minim([X],X).
minim([X|L],R):- length(X,LX), minim(L,R), length(R,LR), LX >= LR.
minim([X|L],X):- length(X,LX), minim(L,R), length(R,LR), LX < LR.

coins(_,0,[]):-!.
coins([X|L],C,[X|M]):- C1 is C - X, C1 >= 0, coins([X|L],C1,M). 
coins([X|L],C,M):- C1 is C - X, C1 >= 0, coins(L,C,M).
coins([X|L],C,M):- C1 is C - X, C1 < 0, coins(L,C,M).

maq(L,C,R):- ord(L,O), coins(O,C,FirstModel), length(FirstModel,LFM),
setof( M, (coins(O,C,M), length(M,LE), LE =< LFM), A), 
minim(A,MIN), !, convertModel(MIN,L,R).

%main:-maq([1,2,5,13,17,35,157],361,L),!, write(L),nl,fail,halt.


% -------------------------------------------------------------------
% -20-

flatten2([],[]).
flatten2([X|L],F):- flatten2(X,F1), flatten2(L,F2), append(F1,F2,F),!.
flatten2(X,[X]).

%main:-flatten2([a,b,[c,[d,[w,w,w]],e,[]],f,[g,h]],F), write(F), nl, fail, halt. 

% -------------------------------------------------------------------
% -21-

log2(_,2,1).
log2(_,N,0):- N < 2.
log2(B,N,L):- N > 2, N1 is N/B, log2(B,N1,R), L is R + 1. 


%main:-log2(2,1020,L), write(L), nl, fail, halt.


% -------------------------------------------------------------------
% -22-
array(0,[]).
array(N,[N|A]):- N1 is N-1, array(N1,A).

subcjto(0,_,[]).  %subcjto(L,S) significa "S es un subconjunto de L".
subcjto(M,[X|C],[X|S]):- M1 is M -1, subcjto(M1,C,S).
subcjto(M,[_|C],S):-subcjto(M,C,S).

subconjunto(M,N,S):- array(N,A), subcjto(M,A,S).

compatibles([],_).
compatibles([[X,Y]|XS],S):- 
	(not(member(Y,S));not(member(X,S))), 
	compatibles(XS,S).

li(_,0,_,[]).
li(N,M,L,S):- subconjunto(M,N,S), compatibles(L,S). 


%main:-li( 20,16,[[8,11],[8,15],[11,6],[4,9],[18,13],[7,9],[16,8],[18,10],[6,17],[8,20]],S), write(S), nl, fail, halt. 

% -------------------------------------------------------------------
% -ALTRES PROBLEMES- swipl

%gana(D1,D2):-findall(X - Y, (member(X,D1), member(Y,D2),X > Y), L)), length(L,K), K >= 5.

dadosColores:-
R = [R1,R2,R3],
V = [V1,V2,V3],
A = [A1,A2,A3], R1 = 1,
permutation([R1,R2,R3,V1,V2,V3,A1,A2,A3],[1,2,3,4,5,6,7,8,9]),
gana(R,V), gana(V,A), gana(A,R).
%main:- dadosColores(R,V,A) (R-V-A), nl, halt.



