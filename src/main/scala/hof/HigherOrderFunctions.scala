package hof

object HigherOrderFunctions extends App {

  def fibonacci(n: Int): Int = {
    def go(currentFib: Int, prev: Int, current: Int): Int = {
      if(currentFib == n)
        current
      else
        go(currentFib + 1, current, current + prev)
    }
    if(n == 1) 0 else go(2, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if(n >= as.length - 1) true
      else{
        if(ordered(as(n), as(n+1)))
          loop(n+1)
        else
          false
      }
    }
    loop(0)
  }

  println(isSorted(Array(1, 2, 3, 5, 4), (first: Int, second: Int) => first < second))
}
