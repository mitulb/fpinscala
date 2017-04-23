import sun.font.TrueTypeFont

import scala.annotation.tailrec

// Exercise 2.1
def fib(n : Int): Int = {
    @tailrec
    def _fib(x: Int, prev: Int, next: Int): Int = x match {
      case 0 => prev
      case 1 => next
      case _ => _fib(x - 1, next, next + prev)
    }
  _fib(n, 0, 1)
}

// Exercise 2.2
def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @tailrec
  def _isSorted(n : Int): Boolean = {
    n match {
      case n if (n >= as.length -1) => true
      case _ => !gt(as(n), as(n+1)) && _isSorted(n+1)
    }
  }
  _isSorted(0)
}

// Exercise 2.3
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b : B) => f(a, b)

// Exercise 2.4
def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a : A) => (b : B) => f(a, b)

// Exercise 2.5
def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a : A, b : B) => f(a)(b)

// Exercise 2.6
def compose[A,B,C](f: B => C, g: A => B): A => C = (a : A) => f(g(a))