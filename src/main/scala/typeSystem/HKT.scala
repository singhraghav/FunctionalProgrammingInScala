package typeSystem

object HKT extends App{
  // make a multiple method for different monads

  implicit class ListMonadMultiplier[A](list: List[A]) extends Monad[List, A] {
    override def flatMap[B](fn: A => List[B]): List[B] = list.flatMap(fn)

    override def map[B](f: A => B): List[B] = list.map(f)
  }

  implicit class OptionMonadMultiplier[A](option: Option[A]) extends Monad[Option, A] {
    override def flatMap[B](fn: A => Option[B]): Option[B] = option.flatMap(fn)

    override def map[B](f: A => B): Option[B] = option.map(f)
  }

  trait Monad[F[_], A]{
    def flatMap[B](fn: A => F[B]): F[B]
    def map[B](f: A => B): F[B]
  }

  def multiply[F[_], A, B](implicit monad1: Monad[F, A], monad2: Monad[F, B]): F[(A, B)] =
    for{
      a <- monad1
      b <- monad2
    } yield (a, b)

  println(multiply(List(1,2,3), List("a", "b")))
  println(multiply(Some(2), Some(3)))

}
