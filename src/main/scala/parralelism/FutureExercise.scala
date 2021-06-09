package parralelism

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

object FutureExercise extends App {
  val futureWithAnImmediateValue: Future[Int] = Future.successful(10)

  def inSequence[A, B](firstFuture: Future[A], secondFuture: Future[B]): Future[B] = {
    for{
      _ <- firstFuture
      second <- secondFuture
    } yield second
  }

  //return whichever future completes first
  def first[A](firstFuture: Future[A], secondFuture: Future[A]): Future[A] = {
    val promise = Promise[A]

    def tryComplete(prom: Promise[A], result: Try[A]) = result match {
      case Success(value) =>
        try{promise.success(value)} catch {case _: Throwable =>}
      case Failure(exception) =>
        try{promise.failure(exception)} catch {case _: Throwable =>}
    }

//    firstFuture.onComplete(tryComplete(promise, _))
//    secondFuture.onComplete(tryComplete(promise, _))

    //use the inbuilt tryComplete
    firstFuture.onComplete(promise.tryComplete)
    secondFuture.onComplete(promise.tryComplete)
    promise.future
  }

//future which finish last
  def last[A](firstFuture: Future[A], secondFuture: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    firstFuture.onComplete{result =>
      if(!bothPromise.tryComplete(result))
        lastPromise.tryComplete(result)
    }

    secondFuture.onComplete{result =>
      if(!bothPromise.tryComplete(result))
        lastPromise.tryComplete(result)
    }
    lastPromise.future
  }
  // retryUntill

  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
//    action().flatMap{ result =>
//      if (condition(result)) {
//        Future.successful(result)
//      } else {
//        retryUntil(action, condition)
//      }
//    }.recoverWith(error => retryUntil(action, condition))

    action().filter(condition).recoverWith{case _ => retryUntil(action, condition)}
  }
}
