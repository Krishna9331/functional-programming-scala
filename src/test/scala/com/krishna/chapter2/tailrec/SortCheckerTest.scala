package com.krishna.chapter2.tailrec

import org.specs2.mutable.Specification

/**
  * Created by mishrk3 on 3/16/2016.
  */
class SortCheckerTest extends Specification {

  "SortChecker" should {
    val sc = new SortChecker
    "return true for Sorted Int Array in Asc" in{
      val as: Array[Int] = Array(1, 4, 5, 7, 9)
      sc.isSorted(as, (x: Int, y: Int) => x < y) mustEqual true
    }
    "return false for not Sorted Int Array in Asc" in{
      val as: Array[Int] = Array(1, 4, 3, 7, 9)
      sc.isSorted(as, (x: Int, y: Int) => x < y) mustEqual false
    }
    "return true for Sorted Int Array in Desc" in{
      val as: Array[Int] = Array(9, 7, 6, 4, 2)
      sc.isSorted(as, (x: Int, y: Int) => x > y) mustEqual true
    }
    "return false for not Sorted Int Array in Desc" in{
      val as: Array[Int] = Array(9, 4, 8, 2, 1)
      sc.isSorted(as, (x: Int, y: Int) => x > y) mustEqual false
    }
    "return true for Sorted String Array in Asc" in{
      val as: Array[String] = Array("Atul", "Bivabh", "Krishna", "Zoo")
      sc.isSorted(as, (x: String, y: String) => x < y) mustEqual true
    }
    "return false for not Sorted String Array in Asc" in{
      val as: Array[String] = Array("Atul", "Foo", "Bivabh", "Krishna", "Zoo")
      sc.isSorted(as, (x: String, y: String) => x < y) mustEqual false
    }
  }

}
