package com.krishna.chapter2.tailrec

/**
  * Created by mishrk3 on 3/17/2016.
  */
class Partial {
  /**
    * a higher-order function for doing what is called
    * partial application. This function, partial1, takes a value and a function of two
    * arguments, and returns a function of one argument as its result. The name comes
    * from the fact that the function is being applied to some but not all of its required
    * arguments.
    *
    * @param a the value
    * @param f function
    * @tparam A first param of function
    * @tparam B second param of function
    * @tparam C output of function
    * @return A function B
    */
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)
}

class Curry {
  /**
    * def g(a: A): (B => C) = {
    * def h(b: B): C = f(a, b)
    * h
    * }
    * g
    * }
    * We can define "h" as value:
    * val h = (b: B) => f(a, b)

    * and get rid of the name altogether leaving function literal, leaving we at:

    * def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    * def g(a: A): (B => C) =
    * (b: B) => f(a, b)
    * g
    * }

    * Then we can define also "g" as value

    * val g = (a: A) =>
    * (b: B) => f(a, b)

    * and again get rid of name leaving only literal, resulting with

    * def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    * (a: A) => (b: B) => f(a, b)
    */

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
}

class UnCurry {

  def uncurry1[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

}
