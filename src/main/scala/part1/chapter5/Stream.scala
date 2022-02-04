package com.stefangiba
package part1.chapter5

import com.stefangiba.part1.chapter5.Stream.cons

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait Stream[+A] {
//  def toList: List[A] = {
//    @tailrec
//    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
//      case Empty => acc.reverse
//      case Cons(head, tail) => go(tail(), head() :: acc)
//    }
//
//    go(this, List())
//  }

  def toList: List[A] = {
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

  def reverse: Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
      case Empty => acc
      case Cons(head, tail) => go(tail(), Cons(head, () => acc))
    }

    go(this, Empty)
  }

  def take(n: Int): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A], times: Int): Stream[A] = stream match {
      case Empty => acc
      case Cons(head, _) if times == 0 => Cons(head, () => acc).reverse
      case Cons(head, tail) => go(tail(), Cons(head, () => acc), times - 1)
    }

    go(this, Empty, n-1)
  }

  def drop(n: Int): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], times: Int): Stream[A] = stream match {
      case Cons(_, tail) if times == 0 => tail()
      case Cons(_, tail) => go(tail(), times - 1)
      case _ => Empty
    }

    if n <= 0 then this
    else go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
      case Cons(head, tail) if p(head()) => go(tail(), cons(head(), acc))
      case _ => acc.reverse
    }

    go(this, Empty)
  }

  def foldRight[B](zero: B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => {
      println(head().toString  + " " + tail().toList.toString())
      val result = f(head(), tail().foldRight(zero)(f))
      println(result)
      result
    }
    case _ => zero
  }

//  def exists(p: A => Boolean): Boolean = this match {
//    case Cons(head, tail) => p(head()) || tail().exists(p)
//    case _ => false
//  }

//  def exists(p: A => Boolean): Boolean = {
//    @tailrec
//    def go(stream: Stream[A], acc: Boolean): Boolean = stream match {
//      case Cons(_, _) if acc => acc
//      case Cons(head, tail) => go(tail(), p(head()) || acc)
//      case _ => acc
//    }
//
//    go(this, false)
//  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = {
    @tailrec
    def go(stream: Stream[A]): Boolean = stream match {
      case Cons(head, tail) if p(head()) => go(tail())
      case Empty => true
      case _ => false
    }

    go(this)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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
  val newStream = Cons(() => expensive(25), () => stream)
//  println(stream.toListFast)
//  println(stream.toList)
//  println(stream.headOption)
  println(newStream.headOption)
  println(newStream.headOption)
  println(newStream.t().toList)
  println(newStream.drop(3).toList)
  println(newStream.takeWhile(_ < 4))
  println(stream.takeWhile(_ < 4).toList)
  println(stream.exists(_ == 1))
  println(stream.forAll(_ < 5))
//  println(stream.take(2).toList)
//  val test = Cons(1, Empty)
//  println(test.toList)
}
