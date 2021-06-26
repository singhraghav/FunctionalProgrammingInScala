package effectivescala

import java.awt.geom.RectangularShape
import java.util.Date

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

  class Person1(val firstName: String = "Raghav", val lastname: String = "Singh")

  object alien {
    def greet(p : Person1) = p.firstName
  }

  object Person1{
    def apply(name: String): Person1 = new Person1(name.split(" ")(0), name.split(" ")(0))
  }

  sealed trait Visitor {
    def id: String
    def createdAt: Date
    def age: Long = new Date().getTime - createdAt.getTime
  }

  final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor
  final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

  trait Feline {
    def color: String
    def sound: String
  }

  case class Cat(color: String, sound: String = "meow", favouriteFood: String) extends Feline
  case class Tiger(color: String, sound: String = "roar") extends Feline
  case class Lion(color: String, sound: String = "roar", maneSize: Int) extends Feline
  case class Panther(color: String, sound: String = "roar") extends Feline

  sealed trait Color {
    def red: Int
    def green: Int
    def blue: Int

    def shade: String = if(red + blue > green) "dark" else "light"
  }

  final case object Red extends Color {
    override def red: Int = 255
    override def blue: Int = 0
    override def green: Int = 0
  }

  final case object Green extends Color {
    override def red: Int = 0
    override def blue: Int = 0
    override def green: Int = 255
  }

  final case object Blue extends Color {
    override def red: Int = 0
    override def blue: Int = 0
    override def green: Int = 255
  }

  final case class CustomColor(red: Int, green: Int, blue: Int) extends Color

  trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
    def color: Color
  }

  sealed trait Rectangular extends Shape {
    def width: Double
    def length: Double
    override val sides = 4
    override def perimeter: Double = 2 * (length + width)
    override def area: Double = length * width
  }

  case class Rectangle(length: Double, width: Double, color: Color) extends Rectangular

  case class Circle(radius: Double, color: Color) extends Shape {
    override def sides: Int = 1
    override def perimeter: Double = 2 * math.Pi * radius
    override def area: Double = math.Pi * radius * radius
  }

  case class Square(side: Double, color: Color) extends Rectangular{
    override val length = side
    override val width = side
  }

  object Draw {
    def apply(shape: Shape) = shape match {
      case Circle(radius, color) => s"A circle of Radius $radius"
      case Square(side, color) => s"A Square with side $side"
      case Rectangle(length, width, color) => s"A Rectangle with length $length and width $width"
    }
  }

  println(Draw(Square(4, Red)))
  println(Draw(Circle(4, Green)))

  sealed trait DivisionResult
  final case class Finite(result: Double) extends DivisionResult
  final case object Infinite extends DivisionResult

  object Division {
    def apply(numerator: Double, denominator: Double): DivisionResult =
      if(denominator.equals(0.0)) Infinite else Finite(numerator / denominator)
  }

  println(Division(1, 2))
  println(Division(2, 0))
}
