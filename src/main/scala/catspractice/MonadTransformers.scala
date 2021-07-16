package catspractice

object MonadTransformers extends App{

  def sumAll(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT

  import cats.instances.list._

  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  val listOfCharOptions:OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    number <- listOfNumOptions
    character <- listOfCharOptions
  } yield (number, character)

  println(listOfTuples.value)


}
