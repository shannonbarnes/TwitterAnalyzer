import TimeRate._

import scala.annotation.tailrec


object TimeRate2 {
  private val second: SecondsInPeriod = 1
  private val minute: SecondsInPeriod = 60
  private val hour: SecondsInPeriod = 3600



  def ratePerSecond(seconds: Int, total: Int): Double = Fraction(total, seconds).roundedDouble()
  def ratePerMinute(seconds: Int, total: Int, avgSec: Double): Double = {

  }

  def calculateRates(seconds: Int, total: Int): (Double, Double, Double) = {
    val avgSec = ratePerSecond(list).roundedDouble()
    (avgSec, ratePerMinute(list, avgSec).roundedDouble(), ratePerHour(list, avgSec).roundedDouble())
  }

}



object TimeRate {

  type CountPerSecond = List[Int]
  type SecondsInPeriod = Int

  private val second: SecondsInPeriod = 1
  private val minute: SecondsInPeriod = 60
  private val hour: SecondsInPeriod = 3600
  private val initFraction = Fraction(0, 1)

  def ratePerSecond(list: CountPerSecond): Fraction = calculateRate(second, list, 0)
  def ratePerMinute(list: CountPerSecond, avgSec: Double): Fraction = calculateRate(minute, list, avgSec)
  def ratePerHour(list: CountPerSecond, avgSec: Double): Fraction = calculateRate(hour, list, avgSec)

  def calculateRates(list: CountPerSecond): (Double, Double, Double) = {
    val avgSec = ratePerSecond(list).roundedDouble()
    (avgSec, ratePerMinute(list, avgSec).roundedDouble(), ratePerHour(list, avgSec).roundedDouble())
  }

  private[this] def calculateRate(period: SecondsInPeriod, list: CountPerSecond, avgSec: Double): Fraction = {

    @tailrec
    def doCalc(list: CountPerSecond, periodLeft: Int, periodSize: Int, f: Fraction, avgSec: Double): Fraction =
      (list, periodLeft) match {
        case (Nil, 0)     => f
        case (Nil, p)     => f.addToNum(p * avgSec)
        case (h :: t, 0)  => doCalc(t, periodSize - 1, periodSize, f.addToNumIncDem(h), avgSec)
        case (h :: t, p)  => doCalc(t, p - 1, periodSize, f.addToNum(h), avgSec)
      }

    doCalc(list, period, period, initFraction, avgSec)

  }

}

