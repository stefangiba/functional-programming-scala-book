package com.stefangiba
package part1.chapter5

object Examples {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if b then j + j else 0
  }

  def main(args: Array[String]): Unit = {
    println(if2(false, sys.error("fail"), 3))
    println(maybeTwice(true, { println("hi"); 1 + 41}))
    println(maybeTwice2(true, { println("hi"); 1 + 41}))
  }
}

