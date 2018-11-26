# Polynomial-simplification

Polynomial simplification program written in prolog.

## Requirements
swipl

## Usage
To open swipl:
```bash
swipl
```

In swipl, first you need to import the program so that you can use the predicates implemented:
```bash
['trab1.pl'].
```

## Description of the program
The program handles 2 types of representations of polynomials: polynomial as an expression and polynomial as a list.
There are 5 main predicates:

   * poly2list/2 - transforms a polynomial into a list, and vice-versa. 
                   
                   ?- poly2list(X,[2,-x]).
                   X = 2-x.
                   ?- poly2list(2*x^2-2*y,X).
                   X = [2*x^2, -2*y].
                   
   * simpoly list/2 -  simplifies a polynomial represented as a list into another polynomial as a list.
   
   
                  ?- simpoly_list([2*x^2,3*x^2,5*x^3],X).
                  X = [5*x^2, 5*x^3].
                  
   * simpoly/2 - simplifies a polynomial represented as an expression as another polynomial as an expression.
   
                  ?- simpoly(2*x^2+3*x^2+5*x^3,X).
                  X = 5*x^2+5*x^3.
                  
   * scalepoly/3 - that multiplies one polynomial as expression by a scalar resulting in a second polynomial. The first 2 arguments are assumed to be ground. The returned polynomial is already simplified.
   
                  ?- scalepoly(2*x^2+3*x^2+5*x^3,2,X).
                  X = 10*x^2+10*x^3.
                  
  * addpoly/3 - adds two polynomials as expressions resulting in a third one. The first 2 arguments are assumed to be ground.
  
                ?- addpoly(2*x^2-5*x,6*x+2*x^3+2*x^2,X).
                X = 4*x^2+x+2*x^3.
 ## Authors
 
   Diogo Pereira 201605323
   
   Ricardo Pereira 201604583
