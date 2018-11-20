

/**
The polynomial is represented as an expression as shown in lec-
tures, using the usual arithmetic operators, ˆfor exponent, lower
case letters such as x, y, ,z etc. for variables, and floating point
notation positive and negative numbers for coefficients. Examples
of polynomials are 2*xˆ2+5+y*2, 7-z and 0.

As a list, the polynomial is decomposed into its monomials and
each is an element of the list. Examples of list polynomials are
[2*x 3ˆ2, 5, y*2], [7, -z] and [0]


poly2list/2
Transforms a list representing a polynomial (second argument) into a polynomial represented as an expression (first argument) and vice-versa


simpoly_list/2
simplifies a polynomial represented as a list into another polynomial as a list


simpoly/2
simplifies a polynomial represented as an expression as another polynomial as an expression. This predicate can use the previous one.


scalepoly/3
multiplies one polynomial as expression by a scalar resulting in a seconed polynomial. The two first arguments are assumed to be ground. The polynomial resulting from the sum is in simplified form.


addpoly/3
adds two polynomials as expressions resulting in a third one. The two first arguments are assumed to be ground. The polynomial resulting from the sum is in simplified form.
*/



pvars([x,y,z]).

pvar(X) :- pvars(V), member(X,V).


power(X) :- pvar(X), !.
power(X^Y) :- pvar(X), integer(Y), Y>1, !.


monomial(N) :- number(N), !.
monomial(X) :- power(X), !.
monomial(-X) :- power(X), !.
monomial(K*X) :- number(K), power(X), !.



merge(X,Y,Z) :- term_to_atom(X,X1), term_to_atom(Y,Y1),
                atom_string(X1,X2), atom_string(Y1,Y2),
                string_chars(X2,X3), string_chars(Y2,Y3), append(X3,Y3,W2),
                string_chars(W,W2), atom_string(Z1,W), term_to_atom(Z,Z1).

list2poly2(M,[M]) :- monomial(M),!.
list2poly2(-M,[-M]) :- monomial(M),!.
list2poly2(P+M,[M|L]) :- monomial(M), split(M,K,_), K>0, 
                         list2poly2(P,L), !.
list2poly2(C,[M|L]) :- monomial(M), split(M,K,_), K<0, 
                       list2poly2(P,L), merge(P,M,C), !.

poly2list2(M,[M]) :- monomial(M), !.
poly2list2(-M,[-M]) :- monomial(M), !.
poly2list2(P+M,[M|L]) :- monomial(M), poly2list2(P,L), !.
poly2list2(P-M,[M2|L]) :- monomial(M), split(M,K,Po),
                         K2 is -K, ((Po==ind,M2=K2);M2=K2*Po),
                         poly2list2(P,L).

poly2list(P,L) :- var(P), reverse(L,L2), list2poly2(P,L2),!.
poly2list(P,L) :- poly2list2(P,L2), reverse(L,L2),!.


%Daqui para cima "aceita" coisas negativas, mas y-2*x+2 fica estranho

%Daqui para baixo so positivos



%Divide monomio em coeficiente e potencia (X^Y)
split(N,N,ind) :- number(N), !.
split(P,1,P) :- power(P), !.
split(-P,-1,P) :- power(P), !.
split(N*P,N,P).

%Dada uma lista de monomios e uma potencia, retorna soma dos quoficientes
%e lista sem monomios com essa potencia
sumE([],_,0,[]) :- !.
sumE([M|P],Po,K,P2) :- split(M,N,Po), sumE(P,Po,N2,P2), K is N+N2, !.
sumE([M|P],Po,K,[M|P2]) :- sumE(P,Po,K,P2).

%Soma todos os monomios com a mesma potencia num so monomio
joinExp([],[]).
joinExp([M|P],[M2|P2]) :- split(M,_,Po), sumE([M|P],Po,K,P3),
                          M2=K*Po, joinExp(P3,P2).

%Corta constantes inuteis e monomios nulos
cutK([],[]).
cutK([M|P],P2) :- split(M,N,_), N is 0, cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), Po=ind, M2=N, cutK(P,P2), !.
cutK([M|P],[M2|P2]) :- split(M,N,Po), N is 1, M2=Po, cutK(P,P2), !.
cutK([M|P],[-M2|P2]) :- split(M,N,Po), N is -1, M2=Po, cutK(P,P2), !.
cutK([M|P],[M|P2]) :- cutK(P,P2).

simpoly_list(L,L2) :- joinExp(L,L3), cutK(L3,L2).



simpoly(P,P2) :- poly2list(P,L), simpoly_list(L,L2), poly2list(P2,L2).



scalepoly2([],_,[]).
scalepoly2([M|P],F,[M2|P2]) :- split(M,N,Po), N2 is N*F, M2=N2*Po,
                               scalepoly2(P,F,P2).

scalepoly(P,F,P2) :- poly2list(P,P3), simpoly_list(P3,P4),
                     scalepoly2(P4,F,P5), simpoly_list(P5,P6), poly2list(P2,P6).


addpoly(P1,P2,R) :- poly2list(P1,P1L), poly2list(P2,P2L), append(P1L,P2L,RL), simpoly_list(RL, RS), poly2list(R,RS). 

append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3). 



























