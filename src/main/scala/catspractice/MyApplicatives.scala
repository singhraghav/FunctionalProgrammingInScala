package catspractice

object MyApplicatives extends App {

  // Applicative == map + pure function from monad
  import cats.syntax.applicative._
  import cats.Applicative

  import cats.instances.list._

  val aList = 2.pure

}
