# Polynomials

A prolog program that allows the user to input verbal actions with polynomials that gives as output a polynomial
representative of the input that the user gave.

## Requirements
swipl

## Usage
To open swipl:
```bash
swipl
```

In swipl, first you need to import the program so that you can use the predicates implemented:
```bash
['trab2.pl'].
```

## Description of the program
The program is given an action with polynomials as a string and outputs a polynomial as an expression.
There are 2 main predicates:

   * text2poly/2 - Given a polynomial as a string, prints the corresponding expression.
                   
          ?- text2poly("two times x plus five plus ten times x squared",X).
          X = 10*x^2+5+2*x.
     
   * polyplay/0 - Creates a new input stream. It does every kind of operation:
      * P - prints the given polynomial P.
      * simplify P - prints the simplification of P.
      * add P1 to P2 - prints the simplified sum of P1 with P2.
      * multiply K by P - prints the polynomial resulting from the multiplication of P by K.
      * show P as S - stores any of the above operations in memory and assigns it a variable.
      * show S - prints variable S if it is stored in memory and the polynomial assigned to it.
      * forget S - removes variable S from memory.
      * show polynomials - shows every variable stored in memory.
   
            ?- polyplay.
            Your operation:
            |: two times x cubed plus five times x raised to two
            5*x^2+2*x^3
            Your operation:
            |: simplify polynomial five plus twenty one
            26
            Your operation:
            |: add two times x to five times x plus two
            7*x+2
            Your operation:
            |: multiply five by five times x plus ten times y
            50*y+25*x
            Your operation:
            |: show two plus five times x as P1
            P1 = 5*x+2
            Your operation:
            |: show two as P1
            P1 is used
            Your operation:
            |: show P1
            P1 = 5*x+2
            Your operation:
            |: show two times x plus five times y as P2
            P2 = 5*y+2*x
            Your operation:
            |: show stored polynomials
            P1 = 5*x+2
            P2 = 5*y+2*x
            Your operation:
            |: forget P1
            Your operation:
            |: show stored polynomials
            P2 = 5*y+2*x
            Your operation:
            |: leave
            Goodbye
            true.
                          
                          
   This last predicate uses the following auxiliar predicates:
                          
   * poly2list/2 - transforms a polynomial as an expression into a polynomial as a list, and vice-versa. 
                   
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
                  
   * scalepoly/3 - multiplies one polynomial as expression by a scalar resulting in a second polynomial. The first 2 arguments are assumed to be ground. The returned polynomial is already simplified.
   
          ?- scalepoly(2*x^2+3*x^2+5*x^3,2,X).
          X = 10*x^2+10*x^3.
                  
  * addpoly/3 - adds two polynomials as expressions resulting in a third one. The first 2 arguments are assumed to be ground.
  
          ?- addpoly(2*x^2-5*x,6*x+2*x^3+2*x^2,X).
          X = 4*x^2+x+2*x^3.
               
 ## Authors
 
   Diogo Pereira 201605323
   
   Ricardo Pereira 201604583
