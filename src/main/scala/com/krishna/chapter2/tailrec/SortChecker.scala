package com.krishna.chapter2.tailrec

import scala.annotation.tailrec

/**
  * Created by mishrk3 on 3/16/2016.
  */
class SortChecker {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

    @tailrec
    def check(low: Int, high: Int): Boolean = {
      val lower = as(low)
      val higher = as(high)
      val greater = gt(lower, higher)
      if (!greater) false
      else if (greater && high != as.length - 1) check(high, high + 1)
      else true
    }
    check(0, 1)
  }

}
