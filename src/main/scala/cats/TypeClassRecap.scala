package cats

object TypeClassRecap extends App {

  sealed trait Json
  final case class JsObject(get: Map[String, Json]) extends Json
  final case class JsString(get: String) extends Json
  final case class JsNumber(get: Double) extends Json
  final case object JsNull extends Json

  // now we want to add a functionality of serializing everything to json
  trait JsonSerializer[T]{
    def serialize(value: T): Json
  }

  case class Person(name: String, age: Int, email: String)

  object JsonSerializerInstances {
    implicit val stringSerializer: JsonSerializer[String] = (value: String) => JsString(value)
    implicit val integerSerializer: JsonSerializer[Int] = (value: Int) => JsNumber(value)
    implicit val personSerializer: JsonSerializer[Person] = (value: Person) => JsObject(Map(
      "name" -> JsString(value.name),
      "age" -> JsNumber(value.age),
      "email" -> JsString(value.email)
    )
    )

    implicit def optionWriter[A](implicit writer: JsonSerializer[A]): JsonSerializer[Option[A]] = {
      case Some(v) => writer.serialize(v)
      case None => JsNull
    }
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonSerializer[A]): Json = w.serialize(value)

    implicit class JsonOps[A](value: A){
      def toJson(implicit w: JsonSerializer[A]) = w.serialize(value)
    }
  }
  /////////////////////Exercise//////////////////////////////
  final case class Cat(name: String, age: Int, color: String)

  trait Printable[T]{
    def format(value: T): String
  }

  object PrintableInstances {
    implicit val stringPrintable: Printable[String] = (value: String) => value
    implicit val intPrintable: Printable[Int] = (value: Int) => value.toString
    implicit val catPrintable: Printable[Cat] = (value: Cat) => s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }

  object Printable{
    implicit class PrintableOps[A](value: A){
      def toString(implicit p: Printable[A]): String = p.format(value)
      def print(implicit p: Printable[A]): Unit = println(p.format(value))
    }
  }

  import Printable._
  import PrintableInstances._
  Cat("Tom", 5, "Black").print
  /////////////////// This is how type classes are implemented in scala
}
