package catspractice

import cats.Id

object MyReader extends App {

  case class Configuration(dbUserName: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(userName: String, password: String) {
    def getOrderStatus(orderId: Long): String = "Dispatched"
  }

  case class HttpService(host: String, port: Int) {
    def start() : Unit = println("server started")
  }

  val config = Configuration("abc", "123", "localHost", 9000, 3, "123@abc.com")

  import cats.data.Reader
  // describe how you will derive the db config from config
  val dbReader: Reader[Configuration, DbConnection] = Reader(config => DbConnection(config.dbUserName, config.dbPassword))
  // actual derivation
  val dbConn: DbConnection = dbReader.run(config)

  def getOrderStatus(id: Long): Reader[Configuration, String] = dbReader.map(conn => conn.getOrderStatus(id))

  getOrderStatus(55).run(config)

  case class EmailService(emailReplyTo: String){
    def sendEmail(address: String, contents: String) = s"Sending email to address $address contents $contents"
  }

  val emailReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))

  def emailUser(userName: String, userEmail: String) = {
    val email = for{
    orderStatus <- dbReader.map(_.getOrderStatus(22))
    email <- emailReader.map(_.sendEmail(userEmail, orderStatus))
    } yield email
    email.run(config)
  }

  println(emailUser("abc", "asdsa"))
}
