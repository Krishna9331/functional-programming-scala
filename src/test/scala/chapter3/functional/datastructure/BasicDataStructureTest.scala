package chapter3.functional.datastructure

import com.krishna.chapter3.functional.datastructure.{Cons, ListStruct}
import org.specs2.mutable.Specification

/**
  * Created by mishrk3 on 3/29/2016.
  */
class BasicDataStructureTest extends Specification {

  "case test" should {
    "return 3" in {
      val x = ListStruct(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + ListStruct.sum(t)
        case _ => 101
      }
      x mustEqual 3
    }
  }

  "sum" should {
    "add all the elements of list" in {
      val ls = ListStruct(1, 4, 6, 9)
      ListStruct.sum(ls) mustEqual 20
    }
  }

  "product" should {
    "multiply all the elements of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.product(ls) mustEqual 48
    }
  }

  "tail" should {
    "truncate first element of list" in {
      val ls = ListStruct(1, 2, 3);
      ListStruct.tail(ls) mustEqual ListStruct(2, 3)
    }
  }

  "drop" should {
    "truncate first n elements of list" in {
      val ls = ListStruct(1, 3, 4, 5, 7);
      ListStruct.drop(ls, 2) mustEqual ListStruct(4, 5, 7)
    }
  }

  "dropWhile" should {
    "drop element until predicate pass" in {
      val ls = ListStruct(2, 4, 6, 5, 8, 10)
      ListStruct.dropWhile(ls)(x => x % 2 == 0) mustEqual ListStruct(5, 8, 10)
    }
  }

  "sethead" should {
    "replace head with provided element" in {
      val ls = ListStruct(3, 7, 9)
      ListStruct.setHead(ls, 2) mustEqual ListStruct(2, 7, 9)
    }
  }

  "append" should {
    "combine the two lists" in {
      ListStruct.append(ListStruct(1, 2), ListStruct(3, 4)) mustEqual ListStruct(1, 2, 3, 4)
    }
  }

  "init" should {
    "remove last element of list" in {
      ListStruct.init(ListStruct(2, 4, 6, 9)) mustEqual ListStruct(2, 4, 6)
    }
  }

  "sum2" should {
    "add all the elements of list" in {
      val ls = ListStruct(1, 4, 6, 9)
      ListStruct.sum2(ls) mustEqual 20
    }
  }

  "product2" should {
    "multiply all the elements of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.product2(ls) mustEqual 48
    }
  }
}
