package typeSystem

object Inheritance extends App {

  trait Writer[T] {
    def write(value: T): Unit
  }

  trait Closable {
    def close(status: Int): Unit
  }

  trait GenericStream[T]{
    def foreach(f: T => Unit): Unit
  }
  // You can mixin directly at the parameter
  def processStream[T](stream: GenericStream[T] with Writer[T] with Closable): Unit = {
    stream.foreach(println)
    stream.close(0)
  }

  //Diamond Inheritance -> Last override is always picked
  trait Animal{def name: String}
  trait Lion extends Animal {override def name: String = "Lion"}
  trait Tiger extends Animal {override def name: String = "Tiger"}

  class Mutant extends Tiger with Lion

  val mu1 = new Mutant
  println(mu1.name) // This will print Lion because it is the last override on mutant and its name function is used

}
