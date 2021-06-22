package subtyping

object SubTypingBasics extends App {
  //1. CoVariant
  //2. InVariant
  //3. Contravariant

  //From Animals
  //Dog, Cat is a subtype of Animal i.e - Dog <: Animal

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
  // Vet[Animal] has become a subtype of Vet[Dog]
  vetForDog.heal(dog1)

  /*
  * use covariance -> you produce collection of things
  * use contravariance -> you produce group of action  (when you use any action)
  * */

  /*
  * val in class parameters are covariant in posiiton and only accept covariant and Invariant type for generic Type
  *var are contravariant when generic type is covariant
  *var are in covariant position when generic type is contravariant
  * var only works for invariant type
  * method argument are in contravariant position
  * method return types are in covariant position
  * */

  trait AnotherCovariantCage[+T]{
    def addAnimal[T1 >: T](animal: T1)
  }

  trait AnotherContravariantCage[-T]{
    def addAnimal(animal: T): Unit
  }

  val catCage: AnotherContravariantCage[Cat] = new AnotherContravariantCage[Animal] {
    override def addAnimal(animal: Animal): Unit = ???
  }

  catCage.addAnimal(new Tiger("ab"))
}
