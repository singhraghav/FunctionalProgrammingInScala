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
  trait Animal{def name: String} // expands to extends AnyRef with <Animal> -> <Animal> is implementation of animal
  trait Lion extends Animal {override def name: String = "Lion"}
  trait Tiger extends Animal {override def name: String = "Tiger"}

  class Mutant extends Tiger with Lion

  val mu1 = new Mutant
  println(mu1.name) // This will print Lion because it is the last override on mutant and its name function is used

  //The super problem -> type linearization
  trait Cold {
    def print = println("Cold")
  }

  trait Green extends Cold {
    override def print: Unit = {
      println("Green")
      super.print
    }
  }

  trait Blue extends Cold {
    override def print: Unit = {
      println("Blue")
      super.print
    }
  }

  class Red {
    def print = println("Red")
  }

  class White extends Red with Green with Blue {
    override def print: Unit = {
      println("White")
      super.print
    }
  }

  val color = new White
  println()
  color.print
  /*
  * White, Blue, Green, Cold
  * Why
  * cold expands to extends AnyRef with <cold>
  * green expands to extends AnyRef with <cold> with <green>
  * blue expands to extends AnyRef with <cold> with <blue>
  * red expands to extends AnyRef with <red>
  * white is extends AnyRef with ( Red - AnyRef with <red>) with ( Green - AnyRef with <cold> with <green>) with (Blue - AnyRef with <cold> with <blue>) with <white>
  * type linearization happens
  * white extends AnyRef with <red> with <cold> with <green> with <blue> with <white>
  * super moves left to write
  * white -> blue -> green -> cold and super ends
  * */
}
