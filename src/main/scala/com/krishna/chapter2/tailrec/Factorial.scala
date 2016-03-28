package com.krishna.chapter2.tailrec

import scala.annotation.tailrec

/**
  * Created by mishrk3 on 2/23/2016.
  */
class Factorial {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

}
