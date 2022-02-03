package com.stefangiba
package part1.chapter4

import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match { // apply f is the option is not None
    case None => None
    case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = { // apply f, which may fail, to the Option if not None
    map(f).getOrElse(None)
  }

  def filter(f: A => Boolean): Option[A] = { // convert Some to None if the value does not satisfy f
    flatMap(value => if f(value) then Some(value) else None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }
  
  def foreach(f: A => Unit): Unit = this match {
    case None => ()
    case Some(value) => f(value)
  }
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
//  def lift[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
//    a.flatMap(a => b.map(b => f(a, b)))

  def lift[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aVal <- a
    bVal <- b
  } yield f(aVal, bVal)

  def sequence[A](seq: Seq[Option[A]]): Option[Seq[A]] = {
    @tailrec
    def go(s: Seq[Option[A]], acc: Option[Seq[A]]): Option[Seq[A]] = (s, acc) match {
      case (_, None) => acc
      case (Seq(), _) => acc
      case _ => go(s.tail, s.head.flatMap(x => acc.map(_ :+ x)))
    }

    go(seq, Some(Seq[A]()))
  }

//  def traverse[A, B](a: Seq[A])(f: A => Option[B]): Option[Seq[B]]
}

