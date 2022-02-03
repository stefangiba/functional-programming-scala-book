package com.stefangiba
package part1.chapter5

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {
  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc.reverse
      case Cons(head, tail) => go(tail(), head() :: acc)
    }

    go(this, List())
  }

  def toListFast: List[A] = {
    val buff = new ListBuffer[A]
    @tailrec
    def go(stream: Stream[A]): List[A] = stream match {
      case Empty => buff.toList
      case Cons(head, tail) => {
        buff += head()
        go(tail())
      }
    }

    go(this)
  }

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(head, tail) => Some(head())
  }

  def take(n: Int): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A], times: Int): Stream[A] = stream match {
      case Empty => acc
      case Cons(head, _) if times == 0 => Cons(head(), acc).reverse
      case Cons(head, tail) => go(tail(), Cons(head(), acc), times - 1)
    }

    go(this, Empty, n-1)
  }

  def reverse: Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
      case Empty => acc
      case Cons(head, tail) => go(tail(), Cons(head(), acc))
    }

    go(this, Empty)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  def this(head: A, tail: Stream[A]) = {
    this(() => head, () => tail)
  }
}

object Cons {
  def apply[A](head: () => A, tail: () => Stream[A]) = new Cons[A](head, tail)
  def apply[A](head:  A, tail: Stream[A]) = new Cons[A](() => head, () => tail)
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => hd, () => tl)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

def expensive(x: Int): Int = {
  println("da")
  x * 2
}

object StreamTest extends App {
  val stream = Stream(1, 2, 3, 4, 5)
  val newStream = Cons(expensive(25), stream)
//  println(stream.toListFast)
//  println(stream.toList)
//  println(stream.headOption)
  println(newStream.headOption)
  println(newStream.headOption)
  println(newStream.t().toList)
//  println(stream.take(2).toList)
//  val test = Cons(1, Empty)
//  println(test.toList)
}
