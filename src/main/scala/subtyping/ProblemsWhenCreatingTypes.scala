package subtyping

object ProblemsWhenCreatingTypes {

   class MyList[+T]{
    def add[S >: T](element: S): MyList[S] = ???
  }

  class Vet[-T]{
    def rescue[S <: T]: S = ???
  }
}
