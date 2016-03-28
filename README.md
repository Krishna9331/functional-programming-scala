##Functional Programming using Scala

This module concentrate on concept of Functional Programming.

The used language is scala for all the program.

It shows how it is different from imperative style of coding and uses pure function through out the exercise.

We can define pure function as functions having no side effects,meaning it will not be doing below things:
*Reassigning a variable
*Modifying a data structure in place
*Setting a field on an object
*Throwing an exception or halting with an error
*Printing to the console or reading user input
*Reading from or writing to a file
*Drawing on the screen

Looks bit strange but yes without doing above we can do every thing in functional programming.

#Other way of defining pure function
    An expression e is referentially transparent if for all programs p, all
    occurrences of e in p can be replaced by the result of evaluating e,
    without affecting the observable behavior of p. 
    A function f is pure if the expression f(x) is referentially transparent for all referentially
    transparent x.

Functional programming buys us great modularity. Modularity means a program consist of Components that can be understood and reused
independently of the whole, such that the meaning of the whole depends only on
the meaning of the components and the rules governing their composition; i.e. they are composable.