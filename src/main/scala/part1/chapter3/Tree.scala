package com.stefangiba
package part1.chapter3

import com.stefangiba.part1.chapter3.Tree.maximum

import scala.annotation.tailrec
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.{Map, Queue}

sealed trait Tree[+A] {
  def size: Int
  def depth: Int
  def map[B](f: A => B): Tree[B]
}
case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def depth: Int = 0

  override def map[B](f: A => B): Tree[B] = Leaf(f(value))
}
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = {
    val toVisit = Queue[Tree[A]](this.left, this.right)
    val visited = Set[Tree[A]](this)

    def go(current: Tree[A], acc: Int): Int = current match {
      case Leaf(value) if toVisit.isEmpty => acc + 1
      case Leaf(value) => {
        visited + current
        go(toVisit.dequeue, acc + 1)
      }
      case Branch(_, _) if visited.contains(current) => go(toVisit.dequeue, acc)
      case Branch(left, right) => {
        visited + current
        toVisit += (left, right)
        go(toVisit.dequeue, acc + 1)
      }
    }

    go(this, 1)
  }

//  override def depth: Int = this match {
//    case Branch(left, right) => 1 + left.depth.max(right.depth)
//  }

  override def depth: Int = {
    val toVisit = Queue[Tree[A]](this)

    @tailrec
    def go(current: Tree[A], level: Queue[Tree[A]], acc: Int): Int = current match {
      case Leaf(_) if (toVisit.isEmpty && level.isEmpty) => acc
      case Leaf(_) if toVisit.isEmpty => {
        toVisit ++= level
        go(toVisit.dequeue, Queue[Tree[A]](), acc + 1)
      }
      case Leaf(_) => go(toVisit.dequeue, level, acc)
      case Branch(left, right) if toVisit.isEmpty => {
        level += (left, right)
        toVisit ++= level
        go(toVisit.dequeue, Queue[Tree[A]](), acc + 1)
      }
      case Branch(left, right) => {
        level += (left, right)
        go(toVisit.dequeue(), level, acc)
      }
    }

    go(toVisit.dequeue, Queue[Tree[A]](), 0)
  }

  override def map[B](f: A => B): Tree[B] = ???
}

object Tree {
  def maximum(t: Tree[Int]): Int = {
    val toVisit = Queue[Tree[Int]](t)

    def go(current: Tree[Int], max: Int): Int = current match {
      case Leaf(value) if toVisit.isEmpty => if (value < max) max else value
      case Leaf(value) => go(toVisit.dequeue, if (value < max) max else value)
      case Branch(left, right) => {
        toVisit += (left, right)
        go(toVisit.dequeue(), max)
      }
    }

    go(toVisit.dequeue(), Int.MinValue)
  }

//  def maximum(t: Tree[Int]): Int = t match {
//    case Leaf(value) => value
//    case Branch(left, right) => maximum(left).max(maximum(right))
//  }
}

object TreeTest extends App {
  val tree: Tree[Int] = new Branch(new Branch(Leaf(1), Leaf(2)), new Branch(Leaf(8), new Branch(Leaf(4), Leaf(5))))
  println(tree.size)
  println(tree.depth)
  println(Tree.maximum(tree))
}
