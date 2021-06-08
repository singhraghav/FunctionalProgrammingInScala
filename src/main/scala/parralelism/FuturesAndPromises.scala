package parralelism

import parralelism.FuturesAndPromises.SocialNetwork.fetchBestFriend

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}

object FuturesAndPromises extends App {

  def calculatingMeaningOfLife = {
    Thread.sleep(2000)
    42
  }

  val aFuture: Future[Int] = Future(calculatingMeaningOfLife)
  println("Waiting on the future")
  aFuture.onComplete{
    case Success(value) => println(s"Future completed successfully $value")
    case Failure(exception) => println(exception)
   }

  Thread.sleep(3000)

  case class Profile(id: String, name: String){
    def poke(another: Profile) = {
      println(s"${this.name} poking ${another.name}")
    }
  }

  object SocialNetwork {
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.1-Bill" -> "Bill",
      "fb.id.1-Dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.1-Bill"
    )

    val random = new Random()
    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
//  val mark: Future[Unit] = SocialNetwork.fetchProfile("fb.id.1-zuck").flatMap(markProfile => fetchBestFriend(markProfile).map(bill => markProfile.poke(bill)))

  val functionalComposition: Future[Unit] = for {
    markProfile <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(markProfile)
  } yield markProfile.poke(bill)

  Thread.sleep(1000)

  //banking app
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      Thread.sleep(400)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      Thread.sleep(500)
      Transaction(user.name, merchantName, amount, "Success")
    }

    def purchase(userName: String, item: String, merchantName: String, cost: Double): String = {
      val transactionFuture = for{
        user <- fetchUser(userName)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionFuture, 2.seconds)

    }
  }

  println(BankingApp.purchase("Raghav", "iphone", "store", 3000))
}


















