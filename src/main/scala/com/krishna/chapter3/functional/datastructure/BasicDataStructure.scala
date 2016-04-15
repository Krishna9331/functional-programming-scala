package com.krishna.chapter3.functional.datastructure

import scala.annotation.tailrec

/**
  * Created by mishrk3 on 3/29/2016.
  */
/**
  * trait is an abstract interface which might has some implemented methods.
  * in below case our trait does not has any methods inside it.
  * Adding sealed in front means that, all implementations of our trait must be declared in this file.
  *
  * The plus sign here indicates that type parameter, A is covariant.
  * Which means it can accept any superType of the declared type. e.g List[Dog] will allow
  * List[Animal] if Dog is subtype of Animal.
  *
  * Here +A also makes the data types of list polymorphic means we can use ListStruct of Int, Double or String.
  *
  * @tparam A
  */
sealed trait ListStruct[+A]

/**
  * One of the possible form of list where it can take nothing - it can be empty.
  */
case object Nil extends ListStruct[Nothing]

/**
  * The another form of List where it consist initial element names as head, followed by other elements called tail
  * which can be empty or can contain rest all element of the list.
  *
  * @param head
  * @param tail
  * @tparam A
  */
case class Cons[+A](head: A, tail: ListStruct[A]) extends ListStruct[A]

/**
  * The object with same name as class is called as Companion objects in Scala.
  *
  * The match keyword behaves like switch statement of java.
  */
object ListStruct {
  def sum(ints: ListStruct[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: ListStruct[Int]): Int = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): ListStruct[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Data is immutable in scala hence for modifying in data structure we always return a new data structure of that type.
    *
    * e.g: When we add an element 1 to the front of an existing list, say xs, we return a new list, in this case
    * Cons(1,xs). Since lists are immutable, we don't need to actually copy xs; we
    * can just reuse it. This property of immutable data is called data sharing or just sharing.
    *
    * similarly to "remove" an element from the front of a list val mylist= Cons(x,xs), we simply return xs.
    * There is no real removing going on. The original list, mylist is still available, unharmed.
    * We say that functional data structures are persistent, meaning that existing references are never changed by
    * operations on the data structure.
    *
    * Using immutable data structures means never having to copy that data just to share it between two components of a system, which
    * promotes keeping these components loosely coupled. We find that in the large, FP can often achieve greater
    * efficiency than approaches that rely on side effects, due to much greater sharing of data and computation.
    */
  def tail[A](ds: ListStruct[A]): ListStruct[A] = ds match {
    case Nil => Nil
    case Cons(_, ds) => ds
  }

  @tailrec
  def drop[A](ds: ListStruct[A], n: Int): ListStruct[A] = (ds, n) match {
    case (Nil, _) => Nil
    case (ds, 0) => ds
    case (ds, n) => drop(tail(ds), n - 1)
  }

  /**
    * The below method has used type inference very well, Since we have applied List as first argument hence
    * scala compiler will automatically identify the type of function data on basis of type of list.
    *
    * (x: Int) => x > 5 is not required only x => x > 5 will do the work.
    *
    * The logic for below solutions start comparing the head with function and drop if it pass otherwise apply the same
    * on tail (which itself is a combination of head and tail)
    */
  @tailrec
  def dropWhile[A](ds: ListStruct[A])(f: A => Boolean): ListStruct[A] = ds match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => ds
  }

  def setHead[A](ds: ListStruct[A], h: A): ListStruct[A] = ds match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Here append method always takes time only for copying the element of a1 and then it will just point to list a2.
    **/
  def append[A](a1: ListStruct[A], a2: ListStruct[A]): ListStruct[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](as: ListStruct[A]): ListStruct[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  /**
    * The below creation of list has been possible only cause of the variadic apply method defined by the companion object
    * ListStruct. Otherwise list creation would have been like Cons(1, Cons(2, Cons(3, Nil))).
    *
    * variadic function meaning it accepts zero or more arguments of type A.
    * For data types, it is a common idiom to have a variadic apply method in the companion
    * object to conveniently construct instances of the data type. By calling
    * this function apply and placing it in the companion object, we can
    * invoke it with syntax like List(1,2,3,4) or List("hi","bye"),
    * with as many values as we want separated by commas
    */
  val example2 = ListStruct(1, 2, 3)


  /**
    * In the above code we have seen there was a duplication of code in method sum and product
    * where apart from operator and return type everything is same.
    *
    * To get rid of duplication we can create a separate function and extract the common code.
    */

  def foldRight[A, B](as: ListStruct[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  //now sum and product

  def sum2(as: ListStruct[Int]) =
    foldRight(as, 0)(_ + _)

  def product2(as: ListStruct[Int]) =
    foldRight(as, 1)(_ * _)
}
