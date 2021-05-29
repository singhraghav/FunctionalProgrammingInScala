package subtyping

//1. CoVariant
//2. InVariant
//3. Contravariant
object SubTyping extends App {
  abstract class Animal{
    def name: String
  }

  case class Dog(name: String) extends Animal
  case class Cat(name: String) extends Animal

  //Dog is a subtype of Animal i.e - Dog <: Animal

  //If Dog is subtype of Animal
  //1. Is List[Dog] also a subtype of List[Animal]
  // If Yes -> Then relation is covariant and is denoted by +T therefore type of list is List[+T]
  // if No -> Then relation is invariant and is denoted by T and if type of List was List[T]
  // then you cannot pass list of dog, cat where list of animal is required
  // if the answer is hell no and the other way around then it is contravariant.
  // denoted by -T. List[Animal] is subtype of List[Dog]

  // covariant -> when my data type produces/ contains data
  // contravariant -> where my data type acts on/consumes data

  //e.g -> contravariant

  abstract class Vet[-T <: Animal]{
    def heal(animal: T): Boolean
  }

  val vetForEveryAnimal: Vet[Animal] = (animal: Animal) => {
    println(animal.name + "healed")
    true
  }

  val dog1 = Dog("Rocky")

  //Now I need a Vet for my dog i.e -> Vet[Dog]
  // but a vet for an animal is also a vet for a dog -> therefore it can be assigned to vet[Dog]

  val vetForDog: Vet[Dog] = vetForEveryAnimal // This is possible because Vet is Contravariant
  vetForDog.heal(dog1)
}
