package com.krishna.chapter2.tailrec

import org.specs2.mutable.Specification

/**
  * Created by mishrk3 on 2/23/2016.
  */
class FibonacciTest extends Specification {

  "fibonacci" should {
    val fib = new Fibonacci
    "give 1st fib value" in {
      fib.fibonacci(1) mustEqual 0
    }
    "give 2nd fib value" in {
      fib.fibonacci(2) mustEqual 1
    }
    "give nth fib value" in {
      fib.fibonacci(5) mustEqual 3
      fib.fibonacci(7) mustEqual 8
      fib.fibonacci(11) mustEqual 55
    }
  }

}
