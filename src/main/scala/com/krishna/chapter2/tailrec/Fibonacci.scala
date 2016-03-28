package com.krishna.chapter2.tailrec

import scala.annotation.tailrec

/**
  * Created by mishrk3 on 2/23/2016.
  */
class Fibonacci {

  def fibonacci(n: Int): Int = {
    /**
      * The Good thing about the annotation tailrec is it auto decides at the compile time
      * is the written code is tail recursion or not
      *
      * @param n the nth fibonacci numbers to be found
      * @param m first fib number
      * @param p second fib number
      */
    @tailrec
    def loop(n: Int, m: Int, p: Int): Int =
      if (n == 1) m
      else if (n == 2) p
      else loop(n - 1, p, m + p)

    loop(n, 0, 1)
  }

}
