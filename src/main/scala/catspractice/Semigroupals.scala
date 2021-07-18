package catspractice

import cats.Monad

object Semigroupals extends App {

  trait MySemigroup[F[_]]{
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }
  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]
  val tuppledOption = optionSemigroupal.product(Some(1), Some("hui"))

  println(tuppledOption)

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def productWithMonad[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] = {
    for{
      a <- fa
      b <- fb
    } yield (a, b)
  }
}
