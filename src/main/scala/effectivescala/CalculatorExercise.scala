package effectivescala

object CalculatorExercise extends App{
  sealed trait Expression {
    def eval: Double = this match {
      case Addition(left, right) => left.eval + right.eval
      case Subtraction(left, right) => left.eval - right.eval
      case Number(value) => value
    }
  }

  final case class Number(value: Double) extends Expression
  case class Addition(left: Expression, right: Expression) extends Expression
  case class Subtraction(left: Expression, right: Expression) extends Expression
  case class Division(left: Expression, right: Expression) extends Expression
  case class SquareRoot(exp: Expression) extends Expression

  sealed trait Json{
    def toJsonString: String = this match {
      case StringJson(value) => "\"" + value + "\""
      case BooleanJson(value) => value.toString
      case IntJson(value) => value.toString
      case DecimalJson(value) => value.toString
      case ListJson(value) => value.map(_.toJsonString).mkString("[", ", ","]")
      case ObjectJson(obj) => obj.map{case (key, value) => StringJson(key).toJsonString +" : " + value.toJsonString}.mkString("{",", ","}")
    }
  }

  case class StringJson(value: String) extends Json
  case class BooleanJson(value: Boolean) extends Json
  case class DecimalJson(value: Double) extends Json
  case class IntJson(value: Int) extends Json
  case class ListJson(value: List[Json]) extends Json
  case class ObjectJson(obj: Map[String, Json]) extends Json
  val obj = Map("a" -> ListJson(List(IntJson(1), IntJson(2), IntJson(3))), "b" -> ListJson(List(StringJson("a"), StringJson("b"), StringJson("c"))))
  println(ObjectJson(obj).toJsonString)
}
