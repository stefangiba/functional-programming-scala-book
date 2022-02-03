package com.stefangiba
package part1.chapter4

object OptionTest{
  def main(args: Array[String]): Unit = {
    val x = mean(List(1, 2, 3))
    println(x.map(_ + 1))
    println(x.flatMap(_ => mean(List())))

    println(lookupByName("Joe").filter(_.name.length != 3).getOrElse(Employee("default", "default")).name)

    println(variance(List(1, 2, 3)).getOrElse(0))

    println(parseInsuranceRateQuote("25", "1").getOrElse(345))

    println(Option.sequence(List(Some(21), Some(22), Some(25))))
    println(parseInts(List("1", "2", "3")))
  }
}

def parseInts(list: List[String]): Option[List[Int]] =
  Option.sequence(list.map(x => Try(x.toInt))).map(_.toList)

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(seq: Seq[Double]): Option[Double] =
  mean(seq).flatMap(m => mean(seq.map(x => math.pow(x - m, 2))))

case class Employee(name: String, department: String)
val company = Map(("Joe" -> Employee("Joe", "Engineering")))
def lookupByName(name: String): Option[Employee] = company.get(name) match {
  case scala.None => None
  case scala.Some(employee) => Some(employee)
}

def insuranseRateQuote(age: Int, numberOfSpeedingTickets: Int): Int = age * numberOfSpeedingTickets

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)

  Option.lift(optAge, optTickets)(insuranseRateQuote)
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch
    case e: Exception => None
