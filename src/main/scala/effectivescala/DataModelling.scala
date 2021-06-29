package effectivescala

import advance_scala.MyMonad.Failure

object DataModelling extends App {

  //Traffic light is Red or Yellow or Green => Sum Type
  sealed trait TrafficLight {
    def next(current: TrafficLight): TrafficLight = current match {
      case Red => Yellow
      case Yellow => Green
      case Green => Red
    }
  }
  case object Red extends TrafficLight
  case object Yellow extends TrafficLight
  case object Green extends TrafficLight


  sealed trait CalculatorResult
  case class Succeed(result: Int) extends CalculatorResult
  case class Failed(reason: String) extends CalculatorResult

  //Algebric Data type is any data which is either a sum type or product type or combination of both

  //Pattern for using algebric data type

  object Calculator {
    def +(calculation: CalculatorResult, element: Int) = calculation match {
      case Succeed(result) => Succeed(result + element)
      case failure => failure
    }

    def -(calculation: CalculatorResult, element: Int) = calculation match {
      case Succeed(result) => Succeed(result - element)
      case failure => failure
    }

    def /(calculation: CalculatorResult, element: Int) = calculation match {
      case Succeed(result) if element == 0 => Failed(s"Can't Divide $result with 0")
      case Succeed(result) => Succeed(result / element)
      case failure => failure
    }
  }

  sealed trait BinaryTree
  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree
  case class Leaf(element: Int) extends BinaryTree

  object BinaryTree {
    def sum(tree: BinaryTree): Int = tree match {
      case Node(l, r) => sum(l) + sum(r)
      case Leaf(e) => e
    }
  }
}
