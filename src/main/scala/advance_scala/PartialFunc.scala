package advance_scala

object PartialFunc extends App {

  val aFunction = (x: Int) => x + 1

  //Restrict the domain of a function

  val aPartialFunction1: PartialFunction[Int, Int] = {
    case 1 => 100
    case 2 => 200
  }

  val aPartialFunction2: PartialFunction[Int, Int] = {
    case 3 => 100
    case 4 => 200
  }

  val combinedFunction = aPartialFunction1 orElse aPartialFunction2
  val lifted = combinedFunction.lift

  val exercise1: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {
    override def isDefinedAt(x: Int): Boolean = x == 1 || x ==2 || x == 3

    override def apply(v1: Int): Int = v1 match {
      case 1 => 100
      case 2 => 200
      case 3 => 300
    }
  }

}
