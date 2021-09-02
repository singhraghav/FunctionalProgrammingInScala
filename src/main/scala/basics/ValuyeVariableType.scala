package basics

import scala.annotation.tailrec

object ValuyeVariableType extends App {
  // variable
  // val - immutable, var - mutable data
  val x: Int = 42
//  x = 43
  var x1 = 45
  x1 = 46
  // Int, Short, Char, Double, Float, String, BigDecimal

  // Expression - > anything that is evaluated to a value

  val y = 1 + 2 + 4 + 5
  val y1: Int = if(true) 42 else 41

  val x3 = {
    val y = 56
    val y2 = 50
    y + y2
  }

  // Functions

  def nameOfFunction(param1: Int) = param1 + 1

  def nestedFunction(x: Int) = {
    def incrementBy: Int = x + 1
    incrementBy
  }

  // Recursive Functions
  //factorial(n) = n * n- 1 * n - 2 * ... * 1
  def factorial(n: Int): Int = if(n <= 1) 1 else n * factorial(n-1)
  //factorial(4) -> 24
  // 4 * 6
  // 3 * 2
  // 2 * 1
  // 1 1 2 3 5

  def fibonaaci(n: Int): Int = if(n <= 2) 1 else fibonaaci(n-1) + fibonaaci(n-2)

  def append(str: String, n: Int): String = if(n == 1) str else str + append(str, n-1)

  println(append("abc", 4))

  //tail recusrsion
  def factorialT(n: Int): Int = {
    @tailrec
    def loop(acc: Int, i: Int): Int = if(i == 1) acc else loop(acc * i, i -1)
    loop(1, n)
  }

  //factorialT(5)
  // loop(1, 5) -> loop(1 * 5, 4) -> loop(5, 4) -> loop(20, 3) -> loop(60, 2) -> loop(120, 1) -> 120
  // fib(n) -> n - 1 , n - 2
  def fibonnaciT(n: Int): Int = {
    @tailrec
    def loop(last: Int, prev: Int, i: Int): Int = if(i == n) last else loop(last + prev, last, i + 1)
    if(n <= 2) 1 else loop(1, 1, 2)
  }
  // 1 1 2 3 5 8
  println(fibonnaciT(6))

  def appendT(str: String, n: Int): String = {
    @tailrec
    def loop(acc: String, i: Int): String = if(i == n) acc else loop(acc + str, i + 1)
    loop(str, 1)
  }

  // call by name and a call by value

  def exampleOfCallByName(cv: Long, cn: => Long) = {
    lazy val x = cn
    println(cv)
    println(cn)
    println(cv)
    println(cn)
    println(s"lazy $x")
    println(s"lazy $x")
    println(s"lazy $x")

  }

  exampleOfCallByName(System.nanoTime(), System.nanoTime())

  //1. variable , expression, functions, recursive functions, tail recursive functions, call by name, call by value

}
