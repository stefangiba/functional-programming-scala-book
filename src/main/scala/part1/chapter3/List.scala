package com.stefangiba
package part1.chapter3

import scala.annotation.tailrec

sealed trait List[+A] {

  def head: A = this match {
    case Nil => throw new NoSuchElementException("Cannot retrieve the head of an empty list")
    case Cons(head, _) => head
  }
  def tail: List[A] = this match {
    case Nil => throw new NoSuchElementException("Cannot retrieve the tail of an empty list")
    case Cons(_, tail) => tail
  }

  def setHead[B >: A](value: B): List[B]
  def drop(n: Int): List[A]
  def dropWhile(predicate: A => Boolean): List[A]
  def append[B >: A](list: List[B]): List[B]
  def append[B >: A](value: B): List[B]
  def reverse: List[A]
  def foldRight[B](initialValue: B)(f: (A, B) => B): B
  def foldLeft[B](initialValue: B)(f: (A, B) => B): B
  def foldLeftViaFoldRight[B](initialValue: B)(f: (B, A) => B): B
  def foldRightViaFoldLeft[B](initialValue: B)(f: (A, B) => B): B
  def lengthViaFoldRight(): Int
  def lengthViaFoldLeft(): Int
  def map[B](f: A => B): List[B]
  def filter(predicate: A => Boolean): List[A]
  def filterViaFlatMap(predicate: A => Boolean): List[A]
  def flatMap[B](f: A => List[B]): List[B]
  def zipWith[B, C](l: List[B])(f: (A, B) => C): List[C]
}
case object Nil extends List[Nothing] {
  override def setHead[B >: Nothing](value: B): List[B] = new Cons(value, Nil)
  override def drop(n: Int): List[Nothing] = Nil
  override def dropWhile(predicate: Nothing => Boolean): List[Nothing] = Nil
  override def append[B >: Nothing](list: List[B]): List[B] = list.reverse.reverse
  override def append[B >: Nothing](value: B): List[B] = setHead(value)
  override def reverse: List[Nothing] = Nil
  override def foldRight[B](initialValue: B)(f: (Nothing, B) => B): B = initialValue
  override def foldLeft[B](initialValue: B)(f: (Nothing, B) => B): B = initialValue
  override def foldLeftViaFoldRight[B](initialValue: B)(f: (B, Nothing) => B): B = initialValue
  override def foldRightViaFoldLeft[B](initialValue: B)(f: (Nothing, B) => B): B = initialValue
  override def lengthViaFoldRight(): Int = 0
  override def lengthViaFoldLeft(): Int = 0
  override def map[B](f: Nothing => B): List[B] = Nil
  override def filter(predicate: Nothing => Boolean): List[Nothing] = Nil
  override def filterViaFlatMap(predicate: Nothing => Boolean): List[Nothing] = Nil
  override def flatMap[B](f: Nothing => List[B]): List[B] = Nil
  override def zipWith[B, C](l: List[B])(f: (Nothing, B) => C): List[C] = Nil
}

case class Cons[+A](h: A, t: List[A]) extends List[A] {
  override def setHead[B >: A](value: B): List[B] = new Cons(value, t)

  override def drop(n: Int): List[A] = {
    def go(times: Int, list: List[A]): List[A] = {
      if (times == 0) list
      else go(times - 1, list.tail)
    }

    go(n, this)
  }

  override def dropWhile(predicate: A => Boolean): List[A] = {
//    def go(list: List[A]): List[A] = {
//      if (!predicate(list.head)) list
//      else go(list.tail)
//    }
    def go(list: List[A]): List[A] = list match {
      case Cons(head, tail) if predicate(head) => go(list.tail)
      case _ => list
    }

    go(this)
  }

//  override def append[B >: A](list: List[B]): List[B] = {
//    def go(current: List[A], acc: List[B]): List[B] = current match {
//      case Cons(head, tail) => go(tail, Cons(head, acc))
//      case Nil => acc
//    }
//
//    go(this.reverse, list)
//  }

  override def append[B >: A](list: List[B]): List[B] = this.reverse.foldLeft(list)(Cons(_, _))

  override def append[B >: A](value: B): List[B] = append(Cons(value, Nil))

//  override def reverse: List[A] = {
//    def go(current: List[A], acc: List[A]): List[A] = current match {
//      case Nil => acc
//      case Cons(head, tail) => go(current.tail, Cons(head, acc))
//    }
//
//    go(this, Nil)
//  }

  override def reverse: List[A] = this.foldLeft(Nil: List[A])(Cons(_, _))

  override def foldRight[B](initialValue: B)(f: (A, B) => B): B = {
    def go(list: List[A]): B = list match {
      case Nil => initialValue
      case Cons(head, tail) => f(head, go(tail))
    }

    go(this)
  }

  override def foldLeft[B](initialValue: B)(f: (A, B) => B): B = {
    def go(list: List[A], acc: B): B = list match {
      case Nil => acc
      case Cons(head, tail) => go(tail, f(head, acc))
    }

    go(this, initialValue)
  }

  override def foldLeftViaFoldRight[B](initialValue: B)(f: (B, A) => B): B =
    foldRight((b: B) => b)((a, g) => b => g(f(b, a)))(initialValue)

  override def foldRightViaFoldLeft[B](initialValue: B)(f: (A, B) => B): B =
    this.reverse.foldLeft(initialValue)(f)

  override def lengthViaFoldRight(): Int = this.foldRight(0)((_, count) => count + 1)

  override def lengthViaFoldLeft(): Int = this.foldLeft(0)((_, count) => count + 1)

  override def map[B](f: A => B): List[B] = {
    @tailrec
    def go(list: List[A], acc: List[B]): List[B] = list match {
      case Nil => acc
      case Cons(head, tail) => go(tail, Cons(f(head), acc))
    }

    go(this.reverse, Nil)
  }

  override def filter(predicate: A => Boolean): List[A] = {
    @tailrec
    def go(list: List[A], acc: List[A]): List[A] = list match {
      case Nil => acc
      case Cons(head, tail) if predicate(head) => go(tail, Cons(head, acc))
      case _ => go(list.tail, acc)
    }

    go(this.reverse, Nil)
  }

  override def filterViaFlatMap(predicate: A => Boolean): List[A] =
    this.map(x => if (predicate(x)) Cons(x, Nil) else Nil).flatMap(x => x)

  override def flatMap[B](f: A => List[B]): List[B] = {
    List.flatten(this.map(f))
  }

  override def zipWith[B, C](list: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def go(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(headA, tailA), Cons(headB, tailB)) => {
        println(headA.toString + " " + headB.toString + " " + f(headA, headB))
        go(tailA, tailB, Cons(f(headA, headB), acc))
      }
    }

    go(this, list, Nil).reverse
  }
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def flatten[A](list: List[List[A]]): List[A] = {
    def go(l: List[List[A]], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(sublist, tail) => go(tail, acc.append(sublist))
    }

    go(list, Nil)
  }
}

object ListTest extends App {
  val list = List(1, 2, 3, 4, 5)
  println(list)
  println(list.head)
  println(list.tail)

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2,Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  println(x)

//  println(list.setHead(5))
//  println(list.drop(4))
//  println(list.dropWhile(_ * 2 < 6))
//  println(list.reverse)
//  println(list.append(1).append(2))
//  println(list.append(List(1, 2, 3)))
//  println(list.foldLeft(0)(_ + _))
//  println(list.foldRight(0)(_ + _))
//  println(list.reverse.foldLeft(List(1, 2, 3, 8, 25))((head, tail) => Cons(head, tail)))
//  println(list.foldRight(Nil: List[Int])(Cons(_, _)))
//  println(list.lengthViaFoldRight())
//  println(list.lengthViaFoldLeft())
//  println(list.foldLeftViaFoldRight(0)(_ + _))
//  println(list.foldRightViaFoldLeft(0)(_ + _))
//  println(List.flatten(List(List(1, 2, 3), List(4, 5, 6), List(6, 7, 8))))
//  println(list.map(_ * 2))
//  println(list.filter(_ % 2 == 0))
//  println(list.flatMap(x => List(x, x+1)))
//  println(list.filterViaFlatMap(_ % 2 == 0))
  println(list.zipWith(List(3, 4, 5))(_ * _))
}
