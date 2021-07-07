package catspractice

import cats.Show
import cats.syntax.show._
import cats._
import cats.instances._
import cats.syntax.eq._

object TypeClassesInCats extends App {
  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val intAsString = 123.show

  case class Person(name: String, age: Int, email: String)
  implicit val personShow: Show[Person] = (v: Person) => s"${v.name}"

  println(Person("Raghav", 22, "abc@g.com").show)

  implicit val personEquality: Eq[Person] = Eq.instance{(p1, p2) =>
      Eq[Int].eqv(p1.age, p2.age) && Eq[String].eqv(p1.name, p2.name)
  }

  println(Person("Raghav", 22, "abc@g.com") === Person("Raghav", 22, "abc@g.com"))

  val option1: Option[Person] =  Option(Person("Raghav", 22, "abc@g.com"))
  val option2: Option[Person] = Option.empty[Person]

  println(option1 === option2)
}
