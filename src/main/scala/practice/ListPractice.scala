package practice

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def headOption: Option[T]
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[T1 >: T](element: T1): RList[T1] = new ::(element, this)
}

case object RNil extends RList[Nothing] {
  override def isEmpty: Boolean = true

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def head: Nothing = throw new NoSuchElementException

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head)

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if(remaining.isEmpty) result
      else if(remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

}

object RList{
  def apply[T](elements: T*): RList[T] =
    if(elements.isEmpty)
      RNil
    else
      elements.head :: apply(elements.tail:_*)
}

object ListPractice extends App {
    println(1 :: 2 :: 3 :: 4 :: RNil)
}
