package advance_scala

object CurriedPaf extends App {

  val supperAdder: Int => Int => Int = x => y => x + y

  val add3: Int => Int = supperAdder(3)
  println(add3(5))

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7_1 = (z: Int) => simpleAddFunction(7, z)
  val add7_2 = curriedAddMethod(7) _
  val add7_3 = (z: Int) => simpleAddMethod(z, 7)

  val curriedFormatter: List[Double] => String => List[String] = (l: List[Double]) => (format: String) => l.map(format.format(_))
  val readyToFormat = curriedFormatter(List(1.23, 4.44, 5.55))
  println(readyToFormat("%8.6f" ))
}
