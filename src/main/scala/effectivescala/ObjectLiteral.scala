package effectivescala

object ObjectLiteral extends App{
  object Oswald{
    val color: String = "Black"
    val food: String = "Milk"
  }

  object Henderson{
    val color: String = "Ginger"
    val food: String = "Chips"
  }

  object Quentin{
    val color: String = "Tabby And White"
    val food: String = "Curry"
  }

  object calc{
    def square(arg: Double): Double = arg * arg
    def cube(arg: Double): Double = square(arg) * arg
  }
  // b c a b a 3 b a

  object Person1 {
    var firstName: String = "Raghav"
    val lastname: String = "Singh"
  }

  object alien {
    def greet(p : Person1.type) = p.firstName
  }
}
