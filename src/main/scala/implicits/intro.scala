package implicits

import scala.language.implicitConversions

object intro extends App {
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name"
  }

  implicit def toPerson(name: String): Person = Person(name)

  println("Raghav".greet)

  //implicit parameters

  def increment(x: Int)(implicit amount: Int) = x + amount

  implicit val defaultAmount = 100

  println(increment(10))
}
