package com.krishna.chapter2.tailrec

import scala.annotation.tailrec

/**
  * Created by mishrk3 on 2/23/2016.
  */
class BinarySearch {

  /**
    *
    * @param as array of Type A
    * @param key The Key to be searched
    * @param gt the method for checking the equality of element with key.
    *           The implementation of the method will be passed by caller.
    *           look at <BinarySearchTest> for details
    * @tparam A The generic type, which can be compared
    * @return The index of the Key in Array.
    */
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

}
