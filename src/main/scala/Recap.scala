object Recap extends  App {
 // method with single param
  def singleArgMethod(arg: Int): String = s"$arg"

  val numbers  = List(1)
  val description = numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ =>
  }

  case class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))
  }

  val bob = new Person("Bob", 25)

  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a yo."
  }

  println(greeting)

  object MathProperty{
    def unapply(number: Int): Option[String] = {
      if(number < 10) Some("Single Digit")
      else if(number % 2 == 0) Some("Event Number")
      else Some("Odd Number")
    }
  }

  object even{
    def unapply(number: Int): Option[Boolean] =
      if(number % 2 == 0) Some(true)
      else None
  }

  object odd{
    def unapply(number: Int): Option[Boolean] =
      if(number % 2 != 0) Some(true)
      else None
  }


  val n: Int = 45
  val mathProperty = n match {
    case even(_) => println(s"$n is even")
    case odd(_) => println(s"$n is odd")
  }

  // infix patterns
  case class Or[A, B](a: A, b: B)
  val either = Or(2, "Two")
  val humanDescription = either match {
    case n Or s => s"$n is written as $s"
  }

  //decomposing sequences
  val varArg = numbers match {
    case List(1, _*) => s"starting with 1"
  }

  abstract class MyList[+A]{
    def head: A
    def tail: MyList[A]
  }

  case object Empty extends MyList[Nothing]{
    override def head: Nothing = throw new Exception("")

    override def tail: MyList[Nothing] = throw new Exception("")
  }
  case class Cons[+A](override val head: A,override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if(list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }
}
