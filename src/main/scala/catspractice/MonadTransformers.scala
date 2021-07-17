package catspractice

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App{

  def sumAll(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT

  import cats.instances.list._
  import cats.instances.future._
  import cats.instances.either._

  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  val listOfCharOptions:OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    number <- listOfNumOptions
    character <- listOfCharOptions
  } yield (number, character)

  println(listOfTuples.value)

  import cats.data.EitherT

  val listOfEitherNum: EitherT[List, String, Int] = EitherT(List(Right(1), Left("Error"), Right(2)))

  val listOfEitherChar: EitherT[List, String, Char] = EitherT(List(Right('c'), Left("Error"), Right('d')))

  val listOfTupleEither: EitherT[List, String, (Int, Char)] = for {
    num <- listOfEitherNum
    char <- listOfEitherChar
  } yield (num, char)

  val result: List[Either[String, (Int, Char)]] = listOfTupleEither.value
  println(listOfTupleEither.value)

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  val bandWidth = Map("server1" -> 50, "server2" -> 300, "server3" -> 170)

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandWidth(server: String): AsyncResponse[Int] = bandWidth.get(server) match {
    case None => EitherT(Future[Either[String, Int]](Left(s"Server $server unavailable")))
    case Some(v) => EitherT(Future[Either[String, Int]](Right(v)))
  }

  def canWithStandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for{
      s1Capacity <- getBandWidth(s1)
      s2Capacity <- getBandWidth(s2)
    } yield (s1Capacity + s2Capacity) > 250
  }

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithStandSurge(s1, s2).transform {
      case Left(value) => Left(value)
      case Right(value) => if(value) Right(s"$s1 and $s2 can with stand spike") else Left(s"$s1 and $s2 can't with stand spike")
    }

  generateTrafficSpikeReport("server1", "server3").value.foreach(println)
  generateTrafficSpikeReport("server1", "server2").value.foreach(println)
  generateTrafficSpikeReport("server5", "server2").value.foreach(println)





















}
