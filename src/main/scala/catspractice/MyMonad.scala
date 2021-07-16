package catspractice

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MyMonad extends App {

  trait MyMonad[M[_]]{
    def pure[A](v: A): M[A]
    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
    def map[A, B](m: M[A])(f: A => B): M[B] = flatMap(m)(a => pure(f(a)))
  }

  val numberList = List(1, 2, 3)
  val charList = List('a', 'b', 'c')

  println(numberList.flatMap(a => charList.map(c => (a, c))))

  val combination: List[(Int, Char)] = for{
    a <- numberList
    c <- charList
  } yield (a, c)

  println(combination)

  import cats.Monad
  import cats.instances.option._
  val optionMonad = Monad[Option]

  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if(x%2 == 0) Option(x%2) else None)

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  import cats.instances.future._
  val futureMonad = Monad[Future]

  val aFuture = futureMonad.pure(10)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x+1)).foreach(println)

  import cats.syntax.applicative._ //pure
  val oneOption = 1.pure[Option]

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  def getPairList[M[_]: Monad, A, B](p1: M[A], p2: M[B]): M[(A, B)] = {
//    p1.flatMap(a => m.map(p2)(b => (a, b)))
    for {
      a <- p1
      b <- p2
    } yield (a, b)
  }

  println(getPairList(Option("1"), Option(2)))
}
