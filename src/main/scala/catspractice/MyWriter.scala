package catspractice

import scala.annotation.tailrec

object MyWriter extends App {

  import cats.data.Writer

  def countAndSay(n: Int): Writer[List[String], Int] = {
    type Computation = Writer[List[String], Int]

    @tailrec
    def tailRec(w: Computation): Computation =
      if(w.value <= 0)
        w.bimap(l => "starting!" :: l, v => v)
      else
        tailRec(w.bimap(l => w.value.toString :: l, v => v - 1))

    tailRec(Writer(List.empty[String], n))
  }

  val (l, c) = countAndSay(10).run
  println(l)
}
