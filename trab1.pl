/**
  Diogo Pereira 201605323
  Ricardo Pereira 201604583
*/


%All possible variables that can be in a polynomial
pvars([x,y,z]).

%check if it's a valid variable
pvar(X) :- pvars(V), member(X,V).

%check if it a lonely variable or if it has an exponent
power(X) :- pvar(X), !.
power(X^Y) :- pvar(X), integer(Y), Y>1, !.

%checks if it is a monomial
monomial(N) :- number(N), !.
monomial(X) :- power(X), !.
monomial(-X) :- power(X), !.
monomial(K*X) :- number(K), power(X), !.

%Divides the monomial in coefficient and exponent (X^Y)
split(N,N,ind) :- number(N), !.
split(P,1,P) :- power(P), !.
split(-P,-1,P) :- power(P), !.
split(N*P,N,P).

%Auxiliar function of list2poly2, concatenates two polynomials without operation sign in the middle
merge(X,Y,Z) :- term_to_atom(X,X1), term_to_atom(Y,Y1),
                atom_string(X1,X2), atom_string(Y1,Y2),
                string_chars(X2,X3), string_chars(Y2,Y3), append(X3,Y3,W2),
                string_chars(W,W2), atom_string(Z1,W), term_to_atom(Z,Z1).

%Auxiliar function of poly2list, converts a polynomial as a list to a polynomial as an expression
list2poly2(M,[M]) :- monomial(M),!.
list2poly2(-M,[-M]) :- monomial(M),!.
list2poly2(P+M,[M|L]) :- monomial(M), split(M,K,_), K>0, 
                         list2poly2(P,L), !.
list2poly2(C,[M|L]) :- monomial(M), split(M,K,_), K<0, 
                       list2poly2(P,L), merge(P,M,C), !.

%Auxiliar function of poly2list, converts a polynomial as an expression to a polynomial as a list
poly2list2(M,[M]) :- monomial(M), !.
poly2list2(-M,[-M]) :- monomial(M), !.
poly2list2(P+M,[M|L]) :- monomial(M), poly2list2(P,L), !.
poly2list2(P-M,[M2|L]) :- monomial(M), split(M,K,Po), K2 is -K,
                          ((Po==ind,M2=K2);(K2 is -1,merge(-,M,M2)); M2=K2*Po),
                          poly2list2(P,L).

%main function to convert polynomials as expressions to polynomials as lists and vice-versa
poly2list(P,L) :- var(P), reverse(L,L2), list2poly2(P,L2),!.
poly2list(P,L) :- poly2list2(P,L2), reverse(L,L2),!.

%Given a list of monomials and an power, returns the sum of the coefficient and the list of monomials with that power
sumE([],_,0,[]) :- !.
sumE([M|P],Po,K,P2) :- split(M,N,Po), sumE(P,Po,N2,P2), K is N+N2, !.
sumE([M|P],Po,K,[M|P2]) :- sumE(P,Po,K,P2).

%Auxiliar function to simpoly_list. Sums the monomials with the same powers
joinExp([],[]).
joinExp([M|P],[M2|P2]) :- split(M,_,Po), sumE([M|P],Po,K,P3),
                          M2=K*Po, joinExp(P3,P2).

%Auxiliar function to simpoly_list. Cuts useless constants and eliminates null monomials
cutK([],[]).
cutK([M|P],P2) :- split(M,N,_), N is 0, cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), Po=ind, M2=N, cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), N is 1, M2=Po, cutK(P,P2), !.
cutK([M|P],[-M2|P2]) :- split(M,N,Po), N is -1, M2=Po, cutK(P,P2), !.
cutK([M|P],[M|P2]) :- cutK(P,P2).

%Simplifies a polynomial as a list
simpoly_list(L,L2) :- joinExp(L,L3), cutK(L3,L2).

%Simplifies a polynomial as an expression
simpoly(P,P2) :- poly2list(P,L), simpoly_list(L,L2), poly2list(P2,L2).


%Multiplies a polynomial by a constant. It returns a simplified polynomial
scalepoly2([],_,[]).
scalepoly2([M|P],F,[M2|P2]) :- split(M,N,Po), N2 is N*F, M2=N2*Po,
                               scalepoly2(P,F,P2).
scalepoly(P,F,P2) :- poly2list(P,P3), simpoly_list(P3,P4),
                     scalepoly2(P4,F,P5), simpoly_list(P5,P6), poly2list(P2,P6).

%Auxiliar function to addpoly. It appends 2 lists, in this case, 2 polynomials
append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).

%It adds 2 polynomials and returns a third simplified polynomial that is the sum of the previous ones.
addpoly(P1,P2,R) :- poly2list(P1,P1L), poly2list(P2,P2L), append(P1L,P2L,RL), simpoly_list(RL, RS), poly2list(R,RS).
