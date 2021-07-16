package catspractice

object Monad extends App{

  case class SafeValue[+T](private val internalValue: T) {
    def get: T = synchronized(internalValue)

    def flatMap[S](transformer: T => SafeValue[S]): SafeValue[S] = synchronized(transformer(internalValue))
  }

  def gimmeSafeValue[T](value: T): SafeValue[T] = SafeValue(value)

  val safeString: SafeValue[String] = gimmeSafeValue("Scala")
  //extract
  val str = safeString.get
  //transform
  val upperCaseStr= str.toUpperCase()
  //wrap
  val upperSafeStr = SafeValue(upperCaseStr)

  //ETW

  //compressed ETW
  val upperSafeStr2 = safeString.flatMap(s => SafeValue(s.toUpperCase()))

//D:\Tutorials
  println(os.root)

  val dRoot = os.Path("D:\\")
  val tutorialsPath = os.Path("Tutorials", base = dRoot)
  println(os.list(tutorialsPath))
}
