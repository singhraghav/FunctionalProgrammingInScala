package catspractice
import cats.syntax.functor._
import cats.instances._

object Functors extends App{
//  val list1 = List(1,2,3)
//  val list2 = Functor[List].map(list1)(_ * 2)
//
//  val doubleList: List[Int] => List[Int] = x => x.map(_*2)
//
//  val liftedList = Functor[List].lift(doubleList)


  // use case for functors is when you want to sequence operations
  // e.g sequencing functions

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"${a}!"

  val func4: Int => String = func1.map(func2).map(func3)

  println(func4(123))

  //continue on page number 72

}
