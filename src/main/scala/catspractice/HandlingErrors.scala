package catspractice

import cats.Monad

object HandlingErrors extends App {

  trait MyMonadError[M[_], E] extends Monad[M] {
    def raiseError[A](e: E): M[A]
  }

  import cats.MonadError
  type ErrorOr[A] = Either[String , A]
  val monadError = MonadError[ErrorOr, String]
}
