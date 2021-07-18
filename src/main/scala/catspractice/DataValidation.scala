package catspractice

import cats.Semigroup

object DataValidation extends App {

  import cats.data.Validated
  import cats.instances.int._
  import cats.instances.list._

  val xa: Validated[String, Int] = Validated.valid(42)
  val anInValid: Validated[String, Int] = Validated.invalid("Error")

  def testPrime(n: Int): Boolean = {
    def tailRecPrime(d: Int): Boolean =
      if(d <= 1) true
      else n %d != 0 && tailRecPrime(d-1)

    if(n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n, List("number should be even"))
      .combine(Validated.cond(n >=0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n , List("Number must be less than 100")))
  }

  println(validateNumber(121).toEither)

  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]
  }
  import cats.syntax.validated._

  val a = 42.validNec

















}
