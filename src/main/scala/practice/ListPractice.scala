package practice

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def headOption: Option[T]
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[T1 >: T](element: T1): RList[T1] = new ::(element, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]

  def ++[T1 >: T](another: RList[T1]): RList[T1]

  def removeAt(index: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def isEmpty: Boolean = true

  override def tail: RList[Nothing] = throw new NoSuchElementException

  override def head: Nothing = throw new NoSuchElementException

  override def headOption: Option[Nothing] = None

  override def toString: String = "[]"

  override def apply(index: Int): Nothing = throw new NoSuchElementException

  override def length: Int = 0

  override def reverse: RList[Nothing] = this

  override def ++[T1 >: Nothing](another: RList[T1]): RList[T1] = another

  override def removeAt(index: Int): RList[Nothing] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def headOption: Option[T] = Some(head)

  override def apply(index: Int): T = {
    @tailrec
    def applyTailRec(remaining: RList[T], currentIndex: Int): T = {
      if(currentIndex == index)
        remaining.head
      else applyTailRec(remaining.tail, currentIndex + 1)
    }
    if(index < 0)
      throw new NoSuchElementException
    else
      applyTailRec(this, 0)
  }

  override def toString: String = {
    @tailrec
    def toStringTailRec(remaining: RList[T], result: String): String = {
      if(remaining.isEmpty) result
      else if(remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

  override def length: Int = {
    @tailrec
    def loop(len: Int, remaining: RList[T]): Int =
      if (remaining == RNil) len
      else loop(len + 1, remaining.tail)

    loop(0, this)
  }

  override def reverse: RList[T] = {
    @tailrec
    def loop(acc: RList[T], remaining: RList[T]): RList[T] =
      if (remaining == RNil) acc
      else loop(remaining.head :: acc, remaining.tail)
    loop(RNil, this)
  }

  override def ++[T1 >: T](another: RList[T1]): RList[T1] = {
    @tailrec
    def loop(acc: RList[T1], remaining: RList[T1]): RList[T1] =
      if (remaining.isEmpty) acc
      else loop(remaining.head :: acc, remaining.tail)

    loop(another, this.reverse)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def loop(acc: RList[T], remaining: RList[T], k: Int): RList[T] = {
      if(remaining.isEmpty || k < 0) this
      else if(k == 0)
        acc.reverse ++ remaining.tail
      else
        loop(remaining.head :: acc, remaining.tail, k -1)
    }

    loop(RNil, this, index)
  }
}

object RList{
  def apply[T](elements: T*): RList[T] =
    if(elements.isEmpty)
      RNil
    else
      elements.head :: apply(elements.tail:_*)

  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def loop(acc: RList[T], remaining: Iterable[T]): RList[T] =
      if (remaining.isEmpty) acc
      else loop(remaining.head :: acc, remaining.tail)
    loop(RNil, iterable).reverse
  }
}

object ListPractice extends App {
  val list = RList(1, 2, 3, 4, 5)
//    println(list.reverse)
//  println(RList.from(1 to 10))

  println((RList(1,2,3) ++ RList(4,5,6)).removeAt(5))

}
