package catspractice


object MyEval extends App {

  import cats.Eval
  //eager evaluation
  val instantEval: Eval[Int] = Eval.now{
    println("Eager Evaluation")
    6345
  }

  //Evaluated always when you request it
  val redoEval = Eval.always{
    println("Computing again")
    343
  }
  //Evaluated when called but only once
  val delayedEval = Eval.later{
    println("Later evaluation")
    434
  }
//  println(delayedEval.value)
//  println(delayedEval.value)

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.defer(eval)

  def reverseList[T](list: List[T]): Eval[List[T]] =
    if(list.isEmpty) Eval.later(list)
    else defer(reverseList(list.tail).map(l => l :+ list.head))

  defer(Eval.now{println("Now!");42})

  println(reverseList((1 to 10000).toList).value)

  //Eval.defer converts a stack recursive function to tail recursive
}
