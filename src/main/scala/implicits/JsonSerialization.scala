package implicits

import java.util.Date

object JsonSerialization extends App {

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, post: List[Post])

  sealed trait JsonValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JsonValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JsonValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JsonValue]) extends JsonValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONObject(values: Map[String, JsonValue]) extends JsonValue {
    override def stringify: String = values.map {
      case (key, value) => "\"" + key + "\":" + value.stringify
    }.mkString("{", ",", "}")
  }

  val data = JSONObject(Map("user" -> JSONString("Raghav"), "posts" -> JSONArray(List(JSONString("raghav Rocks"), JSONNumber(22)))))

  trait JsonSerializer[T] {
    def serialize(a: T): JsonValue
  }

  implicit object StringConverter extends JsonSerializer[String] {
    override def serialize(a: String): JsonValue = JSONString(a)
  }
  implicit object NumberConverter extends JsonSerializer[Int] {
    override def serialize(a: Int): JsonValue = JSONNumber(a)
  }

  implicit object UserConverter extends JsonSerializer[User] {
    override def serialize(a: User): JsonValue = JSONObject(Map("name" -> JSONString(a.name), "age" -> JSONNumber(a.age), "email" -> JSONString(a.email)))
  }

  implicit object PostConverter extends JsonSerializer[Post] {
    override def serialize(a: Post): JsonValue = JSONObject(Map("content" -> JSONString(a.content),
      "createdAt" -> JSONString(a.createdAt.toString)))
  }

  implicit object FeedConverter extends JsonSerializer[Feed] {
    override def serialize(a: Feed): JsonValue = JSONObject(
      Map("user" ->a.user.toJson, "posts" -> JSONArray(a.post.map(_.toJson)))
    )
  }

  implicit class JsonOps[T](value: T) {
    def toJson(implicit converter: JsonSerializer[T]): JsonValue = converter.serialize(value)
    def toJsonString(implicit converter: JsonSerializer[T]): String = converter.serialize(value).stringify
  }

  val now = new Date(System.currentTimeMillis())
  val raghav = User("raghav", 22, "abc@gmail.com")
  val feed = Feed(raghav, List(Post("hello", now), Post("world", now)))

  println(feed.toJsonString)
}
