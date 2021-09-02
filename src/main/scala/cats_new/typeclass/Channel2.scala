package cats_new.typeclass

import java.io.FileOutputStream
import scala.util.Using

trait ByteEncodable {
  def encode(): Array[Byte]
}
trait Channel2 {
  def write(obj: ByteEncodable): Unit
}

case class FullName(firstName: String, lastname: String) extends ByteEncodable {
  override def encode(): Array[Byte] = {
    firstName.getBytes ++ lastname.getBytes
  }
}

object FileChannel2 extends Channel2 {
  override def write(obj: ByteEncodable): Unit = {
  Using(new FileOutputStream("fp-course/test")) { os =>
  os.write(obj.encode())
  os.flush()
}
}
}

//1. unique responsibility
//2 easy to test
//3. compile time error when type mismatch
