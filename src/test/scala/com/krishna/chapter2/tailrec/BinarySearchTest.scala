package com.krishna.chapter2.tailrec

import org.specs2.mutable.Specification

/**
  * Created by mishrk3 on 2/23/2016.
  */
class BinarySearchTest extends Specification {

  "Binary Search" should {
    val bs = new BinarySearch
    "find key of Int type" in {
      val as: Array[Int] = Array(1, 4, 5, 7, 9)
      bs.binarySearch(as, 5, (x: Int, y: Int) => x > y) mustEqual 2
    }
    "find key of String type" in {
      val as: Array[String] = Array("Athul", "Krishna", "Praveen", "tree", "Zoo")
      bs.binarySearch(as, "Krishna", (x: String, y: String) => x > y) mustEqual 1
    }
  }

}
