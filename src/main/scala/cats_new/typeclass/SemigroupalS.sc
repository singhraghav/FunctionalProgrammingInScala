trait MySemigroupal[F[_]]{
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

import cats.Invariant.catsApplicativeForArrow
import cats.data.Validated
import cats.{Applicative, Monad}
import cats.syntax.functor._
import cats.syntax.flatMap._

def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
  for{
  a <- fa
  b <- fb
} yield (a, b)

import cats.Semigroupal
implicit val listSemiGroupal: Semigroupal[List] = new Semigroupal[List] {
  override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
    fa.zip(fb)
}

def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit app: Applicative[W]): W[(A, B)] = {
  //B => (A, B)
  // W[_ => (A, B)]
  val arg1: W[B => (A, B)] = app.map(wa)(a => (b: B) => (a, b))
  app.ap(arg1)(wb)
}

import cats._
import cats.instances._
import cats.syntax.apply._

type IntV[A] = Validated[List[String], A]

val a1: IntV[Int] = Validated.valid(1)
val a2: IntV[Int] = Validated.invalid(List("errror 2"))
val a3: IntV[Int] = Validated.invalid(List("error"))
case class ThreeInt(a1: Int, a2: Int, a3: Int)


val a4  = (a1, a2, a3).mapN(ThreeInt.apply)

println(a4)
















