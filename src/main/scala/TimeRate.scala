import scala.annotation.tailrec
import scala.collection.mutable
import FractionSyntax._

object TimeRate {

  private val seconds = 1
  private val minutes = 60
  private val hour = 3600
  private val initFraction = Fraction(0, 1)

  def ratePerSecond(list: List[Int]): Fraction = calculateRate(seconds, list)
  def ratePerMinute(list: List[Int]): Fraction = calculateRate(minutes, list)
  def ratePerHour(list: List[Int]): Fraction = calculateRate(hour, list)

  def calculateRates(list: List[Int]): (Double, Double, Double) = {
    (ratePerSecond(list).roundedDouble, ratePerMinute(list).roundedDouble, ratePerHour(list).roundedDouble)
  }

  private[this] def calculateRate(bucketSize: Int, list: List[Int]): Fraction = {

    @tailrec
    def doCalc(list: List[Int], bucketLeft: Int, bucketSize: Int, f: Fraction, lastValue: Int): Fraction =
      list match {
        case Nil if bucketLeft == 0 => f
        case Nil => f.addToNum(bucketLeft * lastValue)
        case h :: t if bucketLeft == 0 => doCalc(t, bucketSize - 1, bucketSize, f.addToNumIncDem(h), h)
        case h :: t => doCalc(t, bucketLeft - 1, bucketSize, f.addToNum(h), h)
      }

    doCalc(list, bucketSize, bucketSize, initFraction, 0)

  }



}



class TimeRate {
  val q: mutable.Queue[Int] = mutable.Queue()

  def insert(n: Int) = q.enqueue(n)




}
