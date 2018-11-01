import scala.annotation.tailrec
import scala.collection.mutable
import FractionSyntax._

object TimeRate {

  type CountPerSecond = List[Int]

  private val seconds = 1
  private val minutes = 60
  private val hour = 3600
  private val initFraction = Fraction(0, 1)

  def ratePerSecond(list: CountPerSecond): Fraction = calculateRate(seconds, list)
  def ratePerMinute(list: CountPerSecond): Fraction = calculateRate(minutes, list)
  def ratePerHour(list: CountPerSecond): Fraction = calculateRate(hour, list)

  def calculateRates(list: CountPerSecond): (Double, Double, Double) = {
    (ratePerSecond(list).roundedDouble, ratePerMinute(list).roundedDouble, ratePerHour(list).roundedDouble)
  }

  private[this] def calculateRate(bucketSize: Int, list: CountPerSecond): Fraction = {

    @tailrec
    def doCalc(list: CountPerSecond, bucketLeft: Int, bucketSize: Int, f: Fraction, lastValue: Int): Fraction =
      list match {
        case Nil if bucketLeft == 0 => f
        case Nil => f.addToNum(bucketLeft * lastValue)
        case h :: t if bucketLeft == 0 => doCalc(t, bucketSize - 1, bucketSize, f.addToNumIncDem(h), h)
        case h :: t => doCalc(t, bucketLeft - 1, bucketSize, f.addToNum(h), h)
      }

    doCalc(list, bucketSize, bucketSize, initFraction, 0)

  }

}

