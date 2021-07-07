package effectivescala

object SequencingComputation extends App {
  sealed trait Result[A]
  case class Success[A](result: A) extends Result[A]
  case class Failure[A](reason: String) extends Result[A]


  sealed trait LinkedList[A]{
    def length: Int = this match {
      case End() => 0
      case Cons(_, t) => 1 + t.length
    }

    def contains(e: A): Boolean = this match {
      case End() => false
      case Cons(h, t) => if(h.equals(e)) true else t.contains(e)
    }

    def apply(n: Int): Result[A] = this match {
      case End() if n >= 0 => Failure("Bad things happened")
      case Cons(h, t) => if(n == 0) Success(h) else t.apply(n-1)
    }

    def fold[B](end: B, cons: (A, B) => B): B = this match {
      case End() => end
      case Cons(h, t) => cons(h, t.fold(end, cons))
    }

    def map[B](f: A => B): LinkedList[B] = this match {
      case End() => End()
      case Cons(h, t) => Cons(f(h), t.map(f))
    }
  }
  case class Cons[A](h: A, t: LinkedList[A]) extends LinkedList[A]
  case class End[A]() extends LinkedList[A]

  sealed trait Tree[A] {
    def fold[B](leaf: A => B)(node: (B, B) => B): B = this match {
      case Leaf(element) => leaf(element)
      case Node(left, right) => node(left.fold(leaf)(node), right.fold(leaf)(node))
    }
  }

  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](element: A) extends Tree[A]

  val tree: Tree[String] =
    Node(Node(Leaf("To"), Leaf("iterate")),
      Node(Node(Leaf("is"), Leaf("human,")),
        Node(Leaf("to"), Node(Leaf("recurse"), Leaf("divine")))))

  println(tree.fold((A: String) => A)((C, D) => C + " " + D))

  case class Pair[A, B](a: A, b: B)
  val pair1 = Pair("Hi", 1)

  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C = this match {
      case Left(v) => left(v)
      case Right(v) => right(v)
    }
  }

  case class Left[A, B](value: A) extends Sum[A, B]
  case class Right[A, B](value: B) extends Sum[A, B]

  sealed trait MayBe[+A] {
    def fold[B](empty: B)(f: A => B): B = this match {
      case Empty => empty
      case Full(v) => f(v)
    }

    def flatMap[B](f: A => MayBe[B]): MayBe[B] = this match {
      case Empty => Empty
      case Full(v) => f(v)
    }
  }
  case class Full[A](value: A) extends MayBe[A]
  case object Empty extends MayBe[Nothing]



}
