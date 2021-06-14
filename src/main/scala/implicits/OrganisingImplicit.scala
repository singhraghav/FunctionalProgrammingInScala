package implicits

object OrganisingImplicit extends App{
  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1,4,5,3,2).sorted)

  //scala.Predef is already imported and has implicits

  case class Person(name: String, age: Int)

  val persons = List(Person("Steve", 30), Person("Amy", 22), Person("John", 66))
  println(persons.sorted)

  object Person{
    implicit val sortByName: Ordering[Person] = Ordering.fromLessThan((first, second) => first.name.compareTo(second.name) < 0)
  }
  /*
  * - highest scope - normal scope = Local Scope
  * - imported scope
  * - companion objects of all type involved in method signature
  * */

  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    implicit val totalOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits * a.nUnits < b.nUnits * b.unitPrice)
  }

  object UnitCountOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.nUnits < b.nUnits)
  }

  object UnitPriceOrdering {
    implicit val unitPriceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.unitPrice < b.unitPrice)
  }
}
