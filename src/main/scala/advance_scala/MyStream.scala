package advance_scala

import scala.runtime.Nothing$

abstract class MyStream[+A] {
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]
  def #::[B >: A](element: B): MyStream[B]
  def ++[B >: A](another: MyStream[B]): MyStream[B]
  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(f: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A]
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new Exception("No head in empty Stream")
  def tail: MyStream[Nothing] = throw new Exception("No tail in empty Stream")
  def #::[B >: Nothing](element: B): MyStream[B] = new ConsStream(element, this)
  def ++[B >: Nothing](another: MyStream[B]): MyStream[B] = another
  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(f: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this
  def takeAsList(n: Int): List[Nothing] = List.empty[Nothing]
}

class ConsStream[+A](hd: A, tl: => MyStream[A]) extends MyStream[A]{
  override def head: A = hd
  override lazy val tail: MyStream[A] = tl

  def isEmpty: Boolean = false
  def #::[B >: A](element: B): MyStream[B] = new ConsStream(element, this)
  def ++[B >: A](another: MyStream[B]): MyStream[B] = new ConsStream[B](head, tail ++ another)
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }
  def map[B](f: A => B): MyStream[B] = new ConsStream[B](f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(f: A => Boolean): MyStream[A] = {
    if(f(head)) new ConsStream(head, tail.filter(f))
    else
      tail filter f
  }
  def take(n: Int): MyStream[A] = {
    if(n <=0) EmptyStream
    else if (n == 1) new ConsStream(head, EmptyStream)
    else new ConsStream(head, tail.take(n-1))
  }
  def takeAsList(n: Int): List[A] = {
    def go(n: Int, currentStream: MyStream[A],acc: List[A]): List[A] = {
      if(n == 0 || this.isEmpty) acc
      else
        go(n-1, currentStream.tail, currentStream.head +: acc)
    }
    go(n, this, List.empty[A])
  }
}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = new ConsStream(start, MyStream.from(generator(start))(generator))
}

object MyStreamOps extends App {
  val natural = MyStream.from(1)(_ + 1)
  println(natural)
  println(natural.head)
  println(natural.tail.head)
  println(natural.tail.tail.head)
  val startFromZero = 0 #:: natural
  println(startFromZero.head)
//  startFromZero.take(10).foreach(println)
  println(startFromZero.map(_ * 2).takeAsList(10))
}