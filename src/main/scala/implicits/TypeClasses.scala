package implicits

object TypeClasses extends App {
    trait HTMLWritable {
      def toHtml: String
    }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  val ragahv1 = User("Raghav", 32, "raghav@example.com")
  val ragahv2 = User("Raghav", 42, "raghav@example.com")
  trait Equal[T] {
    def equal(a: T, b: T): Boolean
  }

  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean = equalizer.equal(a, b)
  }

  implicit object NameEquality extends Equal[User] {
    override def equal(a: User, b: User): Boolean = a.name.equals(b.name)
  }

  println(Equal(ragahv1, ragahv2))
}
