package com.stefangiba
package part1.chapter2

object Exercises {

  // EXERCISE 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(idx: Int, acc: Boolean): Boolean = {
      if (idx == as.length || acc == false) acc
      else go(idx + 1, acc && ordered(as(idx - 1), as(idx)))
    }

    go(1, true)
  }

  // EXERECISE 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // EXERCISE 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  //EXERCISE 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (x: A) => f(g(x))
  }

  def main(args: Array[String]): Unit = {
    // EXERCISE 2.2
    println(isSorted(Array(1, 2, 4, 3, 5), (x: Int, y: Int) => x < y))

    // EXERCISE 2.3
    val curriedAdd = curry((a: Int, b: Int) => a + b)
    val add5 = curriedAdd(5)
    println(add5(10))

    // EXERCISE 2.4
    val uncurriedAdd = uncurry(curriedAdd)
    println(uncurriedAdd)

    // EXERCISE 2.5
    println(compose(add5, (x: Int) => x + 1)(5))
  }
}
