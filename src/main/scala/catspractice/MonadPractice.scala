package catspractice

object MonadPractice extends App{

  import cats.Monad
  import cats.instances.list._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.monad._

  val aManualEither: Either[String, Int] = Right(42)

  import cats.instances.either._
  type LoadingOr[T] = Either[String, T]
  val loadingMonad = Monad[LoadingOr]

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to Ship"))

  case class Connection(host: String, port: String)
  val config = Map("host" -> "localhost", "port" -> "4040")

  trait HttpService[M[_]]{
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit n: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response


  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      val connectionBuilder = for{
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      connectionBuilder match {
        case None => Left("Cannot find host/port in config")
        case Some(c) => Right(c)
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      if (payload.length > 20) Left("Bad Payload")
      else Right("Request Complete")
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      for{
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length > 20) None
      else Some("Request Complete")
  }

  println(getResponse(OptionHttpService, "Optional Service"))


}
