import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n == 0) l

    l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  // Exercise 3.4
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => {
      if(f(h)) dropWhile(t)(f)
      else t
    }
  }

  // Exercise 3.5
  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((x, y) => y + 1)
  }

  // Exercise 3.10
  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, Nil) => f(z, x)
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Exercise 3.11
  def sum2(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)
  def product2(ds: List[Double]): Double = foldLeft(ds, 1.0)((x, y) => x * y)
  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => x + 1)

  // Exercise 3.12
  def reverse[A](l : List[A]): List[A] = {
    foldLeft(l, List[A]())((lst, item) => Cons(item, lst))
  }

  // Exercise 3.13
  def foldLeftAsFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, z)((a, b) => f(b, a))
  }
  def foldRightAsFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, z)((b, a) => f(a, b))
  }

  // Exercise 3.14
  def append[A](l : List[A], a : A): List[A] = {
    foldRight(l, List(a))((as, h) => Cons(as, h))
  }

  // Exercise 3.15
  def concatenate[A](l : List[List[A]]): List[A] = {
    foldRight(l, List[A]())((a, b) => append(a, b))
  }
}



