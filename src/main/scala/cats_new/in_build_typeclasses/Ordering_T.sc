import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  val orderById: Order[Account] = Order.from((a1, a2) => (a1.id - a2.id).toInt)
  val orderByBalance: Order[Account] = Order.by(acc => acc.balance)
}

def sort[A](acc: List[A])(implicit orderA: Order[A]): List[A] = acc.sorted(orderA.toOrdering)