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
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  def rle: RList[(T, Int)]
  def duplicateEach(n: Int): RList[T]
  def rotateLeft(n: Int): RList[T]
}

case object RNil extends RList[Nothing] {

  override def rotateLeft(n: Int): RList[Nothing] = this

  override def duplicateEach(n: Int): RList[Nothing] = this

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

  override def map[S](f: Nothing => S): RList[S] = this

  override def filter(f: Nothing => Boolean): RList[Nothing] = this

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = this

  override def rle: RList[(Nothing, Int)] = this
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def rotateLeft(n: Int): RList[T] = {
    @tailrec
    def loop(pending: RList[T], buffer: RList[T], rotationLeft: Int): RList[T] = {
      if(rotationLeft == 0 && pending.isEmpty) this
      else if(pending.isEmpty) loop(this, RNil, rotationLeft)
      else if(rotationLeft == 0) pending ++ buffer.reverse
      else loop(pending.tail, pending.head :: buffer, rotationLeft - 1)
    }

    loop(this, RNil, n)
  }

  override def duplicateEach(n: Int): RList[T] = {
    @tailrec
    def loop(acc: RList[T], pending: RList[T], count: Int): RList[T] = {
      if(pending.isEmpty)
        acc
      else {
        if(count == 0)
          loop(acc, pending.tail, n)
        else
          loop(pending.head :: acc, pending, count - 1)
      }
    }
    if(n <= 1) this
    else
    loop(RNil, this, n).reverse
  }
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

  override def map[S](f: T => S): RList[S] = {
    @tailrec
    def loop(acc: RList[S], remaining: RList[T]): RList[S] =
      if (remaining.isEmpty) acc
      else
        loop(f(remaining.head) :: acc, remaining.tail)
    loop(RNil, this).reverse
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    @tailrec
    def loop(acc: RList[S], pending: RList[T]): RList[S] =
      if (pending.isEmpty) acc
      else
        loop((f(pending.head).reverse ++ acc), pending.tail)

    loop(RNil, this).reverse
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def loop(acc: RList[T], remaining: RList[T]): RList[T] =
      if (remaining.isEmpty) acc
      else
        loop(if(f(remaining.head)) remaining.head :: acc else  acc, remaining.tail)

    loop(RNil, this).reverse
  }

  override def rle: RList[(T, Int)] = {
    @tailrec
    def loop(acc: RList[(T, Int)], pending: RList[T]): RList[(T, Int)] =
      if (pending.isEmpty) acc
      else {
        acc match {
          case h :: tail =>
            if(h._1 == pending.head)
              loop((h._1, h._2 + 1) :: tail, pending.tail)
            else
              loop((pending.head, 1) :: acc, pending.tail)
          case _ => loop((pending.head, 1) :: acc, pending.tail)
        }
      }

    loop(RNil, this).reverse
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
  val list = RList(1, 1, 2, 2, 3, 3, 3, 4, 3, 5, 5, 5)
//    println(list.reverse)
//  println(RList.from(1 to 10))

//  println((RList(1,2,3) ++ RList(4,5,6)).removeAt(5))
// println(RList(1, 2, 3).flatMap(e => RList(e * 2, e * 4)))
//  println(list.rle)

  println(RList(1,2,3).rotateLeft(5))
}
