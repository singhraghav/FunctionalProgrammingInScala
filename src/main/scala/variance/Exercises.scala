package variance

object Exercises extends App{

  class Vehicle
  class Bikes extends Vehicle
  class Cars extends Vehicle

  // Invariant Parking
  trait Thing[T >: Cars <: Vehicle]{}
  class IParking[T](things: List[T]){
    def park(vehicle: T) = println(s"parked Vehicle $vehicle")
    def impound(vehicles: List[T]) = ???
    def checkVehicles(condition: String): List[T] = ???
  }

  //Covariant parking

  class CovParking[+T](things: List[T]) {
    def park[S >: T](vehicle: S) = println(s"parked Vehicle $vehicle")
    def impound[S >: T](vehicles: List[S]) = ???
    def checkVehicles[S >: T](condition: String): List[S] = ???
    def flatMap[S](fn: T => CovParking[S]): CovParking[S] = ???
  }

  //Contravariant parking
  class ConParking[-T](things: List[T]) {
    def park[S <: T](vehicle: S) = println(s"parked Vehicle $vehicle")
    def impound[S <: T](vehicles: List[S]) = ???
    def checkVehicles[S <: T](condition: String): List[S] = ???

    def flatMap[S, R <: T](fn: R => ConParking[S]): ConParking[S] = ???
  }

  val carIParking = new IParking[Cars](List(new Cars, new Cars))
  val carCovParking = new CovParking[Cars](List(new Cars, new Cars))

  val carConParking = new ConParking[Cars](List(new Cars, new Cars))
}
