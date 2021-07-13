package catspractice

object Intro extends App{
  // 1. type class import
  import cats.Eq

  //2. import instances for type i need
  import cats.instances.int._

  //3. type class api
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3)
//  val anUnSafeComparison = intEquality.eqv(2, "a String") -> this wont compile

  //4. use extension methods
  import cats.syntax.eq._
  val typeSafeComparison2 = 2 === 3
  val neqComparison = 1 =!= 3

//  val invalidComparison = 2 === " a string" -> wont compile

  //part 5 - extending operations to composite type
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  println(List(2) === List(3))

  //part 6 -> apply/create instance for custom type
  case class ToyCar(model: String, price: Double)

  implicit val carEq: Eq[ToyCar] = Eq.instance[ToyCar]((car1, car2) => car1.price === car2.price)

  println(ToyCar("1", 10) === ToyCar("2", 10))

  val option1: Option[Int]= Some(2)
  val option2:Option[Int] = None

  val result: Option[Int] = option1.flatMap(a => option2.map(ba => ba/a))
}
