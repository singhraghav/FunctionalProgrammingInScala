package catspractice

object MyMonoid extends App{

  import cats.Monoid
  import cats.instances.int._
  import cats.syntax.monoid._

  def combineFold[T: Monoid](l : List[T]): T = l.foldLeft(Monoid[T].empty)(_ |+| _)

  val phoneBooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 678
    ),
    Map(
      "Dog" -> 910,
      "Emergency" -> 9111
    )
  )

  implicit def monoidMap[A, B]: Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
    override def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] = x ++ y

    override def empty: Map[A, B] = Map[A, B]()
  }

  case class ShoppingCart(items: List[String], total: Double)

  object ShoppingCart {

    implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
      ShoppingCart(List.empty[String], 0.0),
      (c1, c2) => ShoppingCart(c1.items ++ c2.items, c1.total |+| c2.total)
    )

  }

  def checkout(carts: List[ShoppingCart]): ShoppingCart = combineFold(carts)
  println(combineFold(phoneBooks))

  println(checkout(List(ShoppingCart(List("1", "2"), 2), ShoppingCart(List("3", "4"), 4))))

}
