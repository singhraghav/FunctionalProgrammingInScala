import cats._
import cats.implicits._

case class Speed(metresPerSecond: Double) {
  def kiloMetrePerSec: Double = metresPerSecond / 1000.0
  def milesPerSec: Double = metresPerSecond / 1609.34
}

object Speed {
  implicit val speedMonoid: Monoid[Speed] = Monoid.instance(Speed(0.0), (s1, s2) => Speed(s1.metresPerSecond + s2.metresPerSecond))
}

val speed1 = Speed(10)
val speed2 = Speed(20)

val speed3 = speed1 |+| speed2


