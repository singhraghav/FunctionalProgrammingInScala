package parralelism

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object FuturesAndPromises extends App {

  def calculatingMeaningOfLife = {
    Thread.sleep(2000)
    42
  }

  val aFuture: Future[Int] = Future(calculatingMeaningOfLife)
  println("Waiting on the future")
  aFuture.onComplete{
    case Success(value) => println(s"Future completed successfully $value")
    case Failure(exception) => println(exception)
   }

  Thread.sleep(3000)

}
