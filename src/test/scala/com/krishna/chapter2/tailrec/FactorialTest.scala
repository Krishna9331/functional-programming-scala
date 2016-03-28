package com.krishna.chapter2.tailrec

import org.specs2.mutable.Specification

/**
  * Created by mishrk3 on 2/23/2016.
  */
class FactorialTest extends Specification {

  "factorial" should {
    val fac = new Factorial;
    "calculate value" in {
      fac.factorial(5) mustEqual 120

    }
  }
}
