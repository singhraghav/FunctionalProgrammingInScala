import cats_new.typeclass.ByteCodec
import org.scalacheck.{Arbitrary, Test}
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.{exception, forAll}

import java.nio.ByteBuffer

trait ByteCodecLaws[A] {
  def codec: ByteCodec[A]

  def isomorphism(a: A): Boolean = codec.decode(codec.encode(a)).contains(a)
}

trait ByteCodecTests[A] extends Laws {

  def laws: ByteCodecLaws[A]

  def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
    name = "byteCodec",
    parent = None,
    "isomorphism" -> forAll(laws.isomorphism _)
  )

}

object ByteCodecTests {
  def apply[A](implicit ct: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
    override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
      override def codec: ByteCodec[A] = ct
    }
  }
}

implicit val inByteCode: ByteCodec[Int] = new ByteCodec[Int] {
  override def decode(a: Array[Byte]): Option[Int] = {
    if(a.length != 4) None
    else {
      val bb = ByteBuffer.allocate(4)
      bb.put(a)
      bb.flip()
      Some(bb.getInt)
    }
  }

  override def encode(a: Int): Array[Byte] =  {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(a)
    bb.array()
  }
}