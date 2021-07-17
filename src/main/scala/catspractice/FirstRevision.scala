package catspractice

object FirstRevision extends App {

  sealed trait MyList[+T]{
    def fold[T1 >: T](initial: T1)(f: (T1, T1) => T1): T1 = this match {
      case Empty => initial
      case Cons(h, t) => f(h, t.fold(initial)(f))
    }
  }
  case class Cons[+T](head: T, tail: MyList[T]) extends MyList[T]
  case object Empty extends MyList[Nothing]

  trait Vet[-T]{
    def heal[T1 <: T](animal: T1): T1
  }

  trait Animal
  case class Cat(name: String) extends Animal
  case class Dog(name: String, age: Int) extends Animal

  val animalVet: Vet[Animal] = new Vet[Animal] {
    override def heal[T1 <: Animal](animal: T1): T1 = animal match {
      case Cat(name) => println(s"Healed cat named $name");animal
      case Dog(name, _) => println(s"Healed Dog name $name");animal
    }
  }

  val catVet: Vet[Cat] = animalVet

  val healedCat: Cat = catVet.heal(Cat("Whisky"))

  //1. Show type class
  import cats.Show

  implicit val catShow: Show[Cat] = Show.show[Cat](cat => s"Cat with name ${cat.name}")

  import cats.syntax.show._
  println(healedCat.show)

  //2. Eq -> type safe equality

  //scala inbuilt == is not type safe

  println((healedCat == Dog("rocky", 22))) // this will compile although we know that this will always be false
  //Eq class doesn't let the code compile if different types are tried to be equated

  import cats.Eq
  import cats.syntax.eq._
  implicit val catEq: Eq[Cat] = Eq.instance[Cat]((a, b) => a.name === b.name)

//  healedCat === Dog("Ro", 12) because we are using === it wont compile

  println(healedCat === Cat("Whisky"))

  //3. Semigroup[T] -> it is used to combine structures where we don't have identity element

  //1. Reader
  import cats.data.Reader
  import cats.Functor._

  case class Config(username: String, password: String)

  case class HttpServer(config: Config) {
    def orderStatus: String = "On Route"
  }

  case class EmailService(config: Config) {
    def sendEmail(to: String, orderStatus: String): String = s"Send an email to $to by ${config.username} : OrderStatus $orderStatus"
  }
  // login to server get the status if 200 send an email

  val server: Reader[Config, HttpServer] = Reader((c: Config) => HttpServer(c))
  val emailService: Reader[Config, EmailService] = Reader((c: Config) => EmailService(c))

  def email(to: String): Reader[Config, String] = for {
    status <- server.map(_.orderStatus)
    email <- emailService.map(_.sendEmail(to, status))
  } yield email

  println(email("xyz@123.com").run(Config("abc", "123")))

  // Writer -> data structure computes a value and can store intermediate steps

  import cats.data.Writer

  import cats.instances.list._

  type Computation = Writer[List[String], Int]

  val writer1 = Writer(List("Step 1", "Step 2"), 10)
  val writer2 = Writer(List("Step 3"), 20)

  val compositeWriter = for {
    v1 <- writer1
    v2 <- writer2
  } yield (v1, v2)

  val (logs, value) = compositeWriter.run
  println(logs)
  println(value)

  //Eval -> strict evaluation, lazy which is computed once, lazy which is always computed
  // provides map, flatmap, memorize
  // wrapping a stack recursive val in Eval.defer -> converts it into tail recursive

  import cats.Eval

  val strict = Eval.now{print("Strict evaluation"); 42}

  val alwaysEval = Eval.always{println("Always evaluate when called"); 32}

  val newEval: Eval[Int] = for {
    a <- strict
    b <- alwaysEval
  } yield a + b

  println(newEval.value)

  def reverseList[T](l : List[T]): Eval[List[T]] =
    l match {
      case Nil => Eval.now(l)
      case h :: t => Eval.defer(reverseList(t).map(l1 => l1 :+ h))
    }

  println(reverseList((1 to 10000).toList).value)


  //State -> Initial state , Output => define how to compose initial state and compute the output
  //Then you can chain multiple state transition starting from an initial one

  import cats.data.State

  def storeNewDog(dog: Dog): State[List[Dog], Int] = State((d: List[Dog]) => (dog +: d, dog.age))

  val allDogs = for{
    a1 <- storeNewDog(Dog("One", 1))
    a2 <- storeNewDog(Dog("Two", 2))
    a3 <- storeNewDog(Dog("three", 3))
  } yield a1 + a2 + a3

  println(allDogs.run(List()).value)




}
