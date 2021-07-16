package catspractice

import scala.annotation.tailrec

object CustomMonads extends App {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Option[Either[A,B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
    }
  }

  type Identity[T] = T

  object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    def tailRecM[A, B](a: A)(f: A => Identity[Either[A,B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object TreeMonad extends Monad[Tree]{
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

//    def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = f(a) match {
//      case Leaf(Left(value)) => tailRecM(value)(f)
//      case Leaf(Right(value)) => Leaf(value)
//      case Branch(left, right) => Branch(flatMap(left){
//        case Left(value) => tailRecM(value)(f)
//        case Right(value) => pure(value)
//      },
//        flatMap(right){
//        case Left(value) => tailRecM(value)(f)
//        case Right(value) => pure(value)
//      })
//    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A,B]]): Tree[B] = t match {
        case Leaf(Left(value)) => stackRec(f(value))
        case Leaf(Right(value)) => Leaf(value)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }
      stackRec(f(a))
    }
  }
}
