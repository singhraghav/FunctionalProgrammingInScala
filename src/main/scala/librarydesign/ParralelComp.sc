import Prop.{FailedCase, SuccessCount}

trait Gen[A]

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}