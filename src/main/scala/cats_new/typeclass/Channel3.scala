package cats_new.typeclass

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  // summoner
  // is genrally used to use the instance which is in scope
  // e.g - ByteEncoder[Int].encode(2) -> this will look for implicit int instance of ByteEncoder and will call it
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)
}

trait Channel3 {
  def write[A](obj: A)(encoder: ByteEncoder[A]): Unit
}

object FileChannel3 extends Channel3 {
  override def write[A](obj: A)(encoder: ByteEncoder[A]): Unit = {
    Using(new FileOutputStream("fp-course/test")) { os =>
      os.write(encoder.encode(obj))
      os.flush()
    }
  }
}

object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(a: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  }
}

object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] = a.getBytes
}

// can be instance for every type
// cleaner interface
// several implementation possible