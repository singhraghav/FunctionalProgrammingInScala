package catspractice

import cats.{Monoid, Semigroup}
import cats.instances._
import cats.syntax.semigroup._
object MonoidAndSemiGroup extends App{

  val stringResult = "Hi" |+| "There"

//  def add(items: List[Int]): Int = items.reduce(_ |+| _)

  def add[A](items: List[A])(implicit m: Monoid[A]): A = items.foldLeft(m.empty)(_ |+| _)
  println(add(List(1,2,3)))
  println(add(List(Option(1), Option(2), Option(3))))

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order]{
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

    override def empty: Order = Order(0, 0)
  }

  println(add(List(Order(1,2), Order(2, 3), Order(3, 4))))

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemiGroup = Semigroup[Int]

  val intCombination = naturalIntSemiGroup.combine(2, 46)

  def reduce[A](l : List[A])(implicit s: Semigroup[A]): A = l.reduce(s.combine)

  println(intCombination)

  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup = Semigroup.instance[Expense]((e1, e2) => Expense(e1.id + e2.id, e1.amount + e2.amount))

  println(reduce(List(Expense(1, 2), Expense(3, 4), Expense(5, 6))))

  import cats.syntax.semigroup._
  val combinedExpense = Expense(1, 2) |+| Expense(3, 4)

  println(combinedExpense)

  def reduceThings2[T](l: List[T])(implicit s: Semigroup[T]): T = l.reduce(_ |+| _)
}
