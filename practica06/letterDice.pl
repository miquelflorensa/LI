:- use_module(library(clpfd)).

%% A (6-sided) "letter dice" has on each side a different letter.
%% Find four of them, with the 24 letters abcdefghijklmnoprstuvwxy such
%% that you can make all the following words: bake, onyx, echo, oval,
%% gird, smug, jump, torn, luck, viny, lush, wrap.

%Some helpful predicates:
words([[b,a,k,e],[o,n,y,x],[e,c,h,o],[o,v,a,l],[g,i,r,d],[s,m,u,g],[j,u,m,p],
      [t,o,r,n],[l,u,c,k],[v,i,n,y],[l,u,s,h],[w,r,a,p]]).
word( [b,a,k,e] ).
word( [o,n,y,x] ).
word( [e,c,h,o] ).
word( [o,v,a,l] ).
word( [g,i,r,d] ).
word( [s,m,u,g] ).
word( [j,u,m,p] ).
word( [t,o,r,n] ).
word( [l,u,c,k] ).
word( [v,i,n,y] ).
word( [l,u,s,h] ).
word( [w,r,a,p] ).

num(X,N):- nth1( N, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,r,s,t,u,v,w,x,y], X ).



constraints([],[]).
constraints([Letter|Word],Daus):-
    num(Letter,N), member(Dau,Daus), member(N,Dau), select(Dau,Daus,Daus2),
    constraints(Word,Daus2).

applyConstraints([],_,_,_,_).
applyConstraints([Word|Words],D1,D2,D3,D4):-
    constraints(Word,[D1,D2,D3,D4]),
    applyConstraints(Words,D1,D2,D3,D4).



main:-
    length(D1,6),
    length(D2,6),
    length(D3,6),
    length(D4,6),

    D1 ins 1..24,
    D2 ins 1..24,
    D3 ins 1..24,
    D4 ins 1..24,

    % Hem de regstringir que per a cada word, hi hagi 4 on cadascun
    % d'ells contingui una lletra diferent de la paraula

    words(Words),
    applyConstraints(Words,D1,D2,D3,D4),

    labeling([ff],D1),
    labeling([ff],D2),
    labeling([ff],D3),
    labeling([ff],D4),

    writeN(D1),
    writeN(D2),
    writeN(D3),
    writeN(D4), halt.

writeN(D):- findall(X,(member(N,D),num(X,N)),L), write(L), nl, !.
