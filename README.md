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
     
   * polyplay/0 - Creates a new input stream. It does every kind of operation
   (S - name of polynomial, P - polynomial as string or name (if stored), K integer as string):
      * show P - prints the polynomial P.
      * simplify P - prints the simplification of P.
      * add P1 to P2 - prints the simplified sum of P1 with P2.
      * multiply K by P - prints the polynomial resulting from the multiplication of P by K.
      * C as S - stores any of the above operations C in memory and assigns it a variable.
      * forget S - removes variable S from memory.
      * show polynomials - shows every variable stored in memory.
   
            ?- polyplay.
            Your operation:
            |: show five point one times x plus five x as P1 and show ten y squared as P2
            P1 = 5*x+5.1*x
            P2 = 10*y^2
            Your operation:
            |: simplify P1
            10.1*x
            Your operation:
            |: add twenty one times x plus five hundred and twenty one y squared to five x minus ten times x
            521*y^2+16*x
            Your operation:
            |: add ten times x plus five times y cubed to five times y squared plus five
            5*y^3+10*x+5+5*y^2
            Your operation:
            |: multiply ten by P2
            100*y^2
            Your operation:
            |: show polynomials
            P1 = 5*x+5.1*x
            P2 = 10*y^2
            Your operation:
            |: forget P1 and show polynomials
            P2 = 10*y^2
            Your operation:
            |: leave
            Goodbye
            true.
      
      There is a built in feature that shows all the informations about the operations you can do, which can be accessed by typing 'help'. To leave just type 'leave'.           
                          
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
