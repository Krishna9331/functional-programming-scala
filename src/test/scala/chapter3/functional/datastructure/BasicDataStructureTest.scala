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

  "length" should {
    "return length of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.length(ls) mustEqual 3
    }
  }

  "sumUsingFoldLeft" should {
    "add all the elements of list" in {
      val ls = ListStruct(1, 4, 6, 9)
      ListStruct.sumUsingFoldLeft(ls) mustEqual 20
    }
  }

  "productUsingFoldLeft" should {
    "multiply all the elements of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.productUsingFoldLeft(ls) mustEqual 48
    }
  }

  "lengthUsingFoldLeft" should {
    "return length of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.lengthUsingFoldLeft(ls) mustEqual 3
    }
  }

  "reverse" should {
    "return reverse of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.reverse(ls) mustEqual ListStruct(6, 4, 2)
    }
  }

  "appendUsingFoldRight" should {
    "combine the two lists" in {
      ListStruct.appendUsingFoldRight(ListStruct(1, 2), ListStruct(3, 4)) mustEqual ListStruct(1, 2, 3, 4)
    }
  }

  "concatUsingFoldRight" should {
    "combine the lists into single" in {
      val ls = ListStruct[ListStruct[Int]](ListStruct(1, 2), ListStruct(3, 4), ListStruct(7, 9))
      ListStruct.concatUsingFoldRight(ls) mustEqual ListStruct(1, 2, 3, 4, 7, 9)
    }
  }

  "addOneToElement" should {
    "add 1 to each element of list" in {
      val ls = ListStruct(2, 4, 6)
      ListStruct.addOneToElement(ls) mustEqual ListStruct(3, 5, 7)
    }
  }

  "doubleToString" should {
    "convert element of list to String" in {
      val ls = ListStruct(2.0, 4.2, 6.4)
      ListStruct.doubleToString(ls) mustEqual ListStruct("2.0", "4.2", "6.4")
    }
  }

  "map" should {
    "apply function on each element of list" in {
      val ls = ListStruct(2.0, 4.2, 6.4)
      ListStruct.doubleToString(ls) mustEqual ListStruct("2.0", "4.2", "6.4")
    }
  }

  "filter" should {
    "remove element returning false for predicate function" in {
      val ls = ListStruct(2, 5, 8, 6, 3)
      ListStruct.filter(ls)(x => x % 2 == 0) mustEqual ListStruct(2, 8, 6)
    }
  }

  "flatMap" should {
    "apply function and merge the list" in {
      val ls = ListStruct(2, 5, 8)
      ListStruct.flatMap(ls)(x => ListStruct(x, x)) mustEqual ListStruct(2, 2, 5, 5, 8, 8)
    }
  }

  "filterUsingFlatMap" should {
    "apply function and filter the list" in {
      val ls = ListStruct(2, 5, 8, 6, 9, 12)
      ListStruct.flatMap(ls)(x => if (x % 2 == 0) ListStruct(x) else ListStruct()) mustEqual ListStruct(2, 8, 6, 12)
    }
  }

  "applyPairWise" should {
    "apply addition on corresponding element on list" in {
      val ls = ListStruct(2, 5, 8, 6, 9, 12)
      val ls1 = ListStruct(1, 3, 5, 4, 6, 2)
      ListStruct.applyPairWise(ls, ls1)((x, y) => x + y) mustEqual ListStruct(3, 8, 13, 10, 15, 14)
    }
  }

  "applyPairWise" should {
    "combine the corresponding element on list" in {
      val ls = ListStruct(2, 5, 8)
      val ls1 = ListStruct(1, 3, 5)
      ListStruct.applyPairWise(ls, ls1)((x, y) => (x, y)) mustEqual ListStruct((2, 1), (5, 3), (8, 5))
    }
  }

  "startWith" should {
    "return true for sub sequence it starting with" in {
      val ls = ListStruct(2, 5, 8, 7)
      val ls1 = ListStruct(2, 5)
      ListStruct.startsWith(ls, ls1) mustEqual true
    }
  }

  "hasSubsequence" should {
    "return true for valid sub sequence" in {
      val ls = ListStruct(2, 5, 8, 6, 9, 11)
      val ls1 = ListStruct(8, 6, 9)
      ListStruct.hasSubsequence(ls, ls1) mustEqual true
    }
  }
}
