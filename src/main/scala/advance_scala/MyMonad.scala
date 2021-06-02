package advance_scala

trait MyMonad[A] {
  def unit(value: A): MyMonad[A] // apply method
  def flatMap[B](f: A => MyMonad[B]): MyMonad[B] // bind method
}

object MyMonad extends App {
  trait Attempt[+A]{
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = try {f(value)} catch {case ex: Throwable => Failure(ex)}
  }

  case class Failure(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  object Attempt {
    def apply[A](value: => A): Attempt[A] = try {Success(value)} catch {case ex: Throwable => Failure(ex)}
  }

  //left associativity
  // unit(x).flatMap(f) == f(x)
  val f: Int => List[Int] = (x: Int) => List(x)
  //List(1).flatMap(f) expands to  f(1) ++ Nil.flatMap(f)
  val leftAssociativity = List(1).flatMap(f) == f(1)

  //right associativity
  // aMonad.flatMap(unit) == aMonad
  val rightAssociativity = List(1).flatMap(x => List(x)) == List(1)

  //associativity
  //monad.flatMap(g).flatMap(g) == monad.flatMap(x => f(x).flatMap(g))
  val g: Int => List[Int] = (x: Int) => List(2*x)

  val associative = List(1,2,3).flatMap(f).flatMap(g) == List(1,2,3).flatMap(x => f(x).flatMap(g))
}
