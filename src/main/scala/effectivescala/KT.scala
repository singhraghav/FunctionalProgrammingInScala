package effectivescala

object KT extends App {
  //vals , type, vars

  val x = 42
  // val/var name: type of variable = value

  var x1 = 41
  x1 = 42

  val b: Boolean = true
  val f: Float = 3.0f
  val c: Char = 'c'
  val d: Double = 3.045

  // 1. How do you declare variable in scala

  //Expression and Instructions
  //Expression -> anything which evaluates to a value

  val x2 = 1 + 2
  val x4 = if(x < 3) true else false

  val x5 = {
    val first = "something"
    42
  }



}
