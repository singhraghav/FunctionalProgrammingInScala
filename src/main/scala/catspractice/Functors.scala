package catspractice
import cats.Functor
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
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)
  final case class Box[A](value: A)
  val box = Box[Int](123)

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) =>
            Branch(map(left)(f), map(right)(f))
          case Leaf(value) =>
            Leaf(f(value))
        }
    }

  val t= Functor[Tree].map(Branch(Leaf(10), Leaf(20)))(v => v * 2)

}
