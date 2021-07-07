package effectivescala

object Collections {

  val opt1 = Some(1)
  val opt2 = Some(2)
  val opt3 = Some(3)

  for{
    a <- opt1
    b <- opt2
    c <- opt3
  } yield a + b + c


}
