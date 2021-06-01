package advance_scala

object LazyEvaluation extends App {

  lazy val x: Int = throw new RuntimeException

  def sideEffectCondition: Boolean = {
    println("Boolean")
    true
  }

  def simpleCondition: Boolean = false
  lazy val lazyCondition = sideEffectCondition

  println(if(simpleCondition && lazyCondition) "yes" else "no")

  // in conjuction in call by name
  def byName(n: => Int): Int = n + n + n + 1

  def retrieveMagicValue = {
    Thread.sleep(1000)
    42
  }

//  println(byName(retrieveMagicValue))

  // filtering with lazy val

  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)

  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20)
  println("//////////////////////////////////////")
  println(gt20Lazy)

  gt20Lazy.foreach(println)
}
