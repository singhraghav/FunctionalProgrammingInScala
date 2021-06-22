package subtyping


  abstract class Animal{
    def name: String
  }

  case class Dog(name: String) extends Animal
  case class Cat(name: String) extends Animal

//  case class Tiger(override val name: String) extends Cat(name)
