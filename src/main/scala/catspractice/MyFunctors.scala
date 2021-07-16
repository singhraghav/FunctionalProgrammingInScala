package catspractice

object MyFunctors extends App{
  // Functors are a type class which generalize the map function

  trait MuFunc[F[_]]{
    def map[A, B](initialV: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._

  val listFunctor = Functor[List]
  val incrementedList = listFunctor.map(List(1, 2, 3))(_ + 1)
  import cats.syntax.functor._

  def do10X[F[_] : Functor](m: F[Int]): F[Int] = m.map(_ * 10)

  trait Tree[+T]{
    def fold[B](l: T => B, b: (T, B, B) => B): B = this match {
      case Leaf(v) => l(v)
      case Branch(value, left, right) => b(value, left.fold(l, b), right.fold(l, b))
    }
  }

  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
    fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  println(do10X[Tree](Branch(10, Leaf(5), Leaf(5))))

}
