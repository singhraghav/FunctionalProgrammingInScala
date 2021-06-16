package implicits

object TypeClasses extends App {

  trait Equal[T] {
    def apply(first: T, second: T): Boolean
  }

  object Equal{
    def apply[T](first: T, second: T)(implicit equality: Equal[T]): Boolean = equality.apply(first, second)
    def apply[T](implicit equality: Equal[T]) = equality
  }

  implicit object PersonEquality extends Equal[Person] {
    override def apply(first: Person, second: Person): Boolean = first.name.equals(second.name) && first.age == second.age
  }

  case class Person(name: String, age: Int)

  val person1 = Person("Raghav", 22)
  val person2 = Person("Raghav", 22)

  println(Equal(person1, person2))
  println(Equal[Person].apply(person1, person2))

}
