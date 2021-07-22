package revision

object MyVariance extends App {

  trait Animal
  class Cat extends Animal
  class Dog extends Animal

  class ICage[T](animal: T) // if class parameter type is declared invariant then the class parameter can be have the invariant type

  class CoCage[+T](val animal: T) // if the parameter type is covariant the val field can be covariant
}
