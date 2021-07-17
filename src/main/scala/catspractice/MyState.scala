package catspractice

object MyState extends App {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  case class ShoppingCart(items: List[String], total: Double)

  def addedToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State((cart: ShoppingCart) => (ShoppingCart(cart.items :+ item, cart.total + price), cart.total + price))

  val cart1 = for {
    _ <- addedToCart("guitar", 500)
    _ <- addedToCart("milk", 10)
    total <- addedToCart("fluff", 30)
  } yield total

  println(cart1.run(ShoppingCart(List.empty, 0)).value)

  def inspect[A, B](f: A => B): State[A, B] = State{ initial => (initial, f(initial))}

  def get[A]: State[A, A] = State((initial: A) => (initial, initial))

  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  def modify[A](f: A => A): State[A, Unit] = State((in: A) => (f(in), ()))


}
