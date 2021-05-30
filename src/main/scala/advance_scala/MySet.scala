package advance_scala

trait MySet[A] extends (A => Boolean) {

  def contains(element: A): Boolean
  def +(element: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(f: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  override def apply(v1: A): Boolean = contains(v1)
}

class EmptySet[A] extends MySet[A]{
  def contains(element: A): Boolean = false
  def +(element: A): MySet[A] = new NonEmptySet[A](element, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(f: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(element: A): Boolean =  if(head == element || tail.contains(element)) true else false
  def +(element: A): MySet[A] = if(this.contains(element)) this else new NonEmptySet[A](element, this)
  def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(f: A => Boolean): MySet[A] = {
    val filteredTail = tail filter f
    if(f(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
}
object MySet{
  def apply[A](values: A*): MySet[A] = {
    def buildSet(valueSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valueSeq.isEmpty) acc
      else {
        buildSet(valueSeq.tail, acc + valueSeq.head)
      }
    buildSet(values, new EmptySet[A])
  }

}
