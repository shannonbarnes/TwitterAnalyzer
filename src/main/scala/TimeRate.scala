import scala.annotation.tailrec

object TimeRate {

  type CountPerSecond = List[Int]
  type SecondsInPeriod = Int

  private val second: SecondsInPeriod = 1
  private val minute: SecondsInPeriod = 60
  private val hour: SecondsInPeriod = 3600
  private val initFraction = Fraction(0, 1)

  def ratePerSecond(list: CountPerSecond): Fraction = calculateRate(second, list)
  def ratePerMinute(list: CountPerSecond): Fraction = calculateRate(minute, list)
  def ratePerHour(list: CountPerSecond): Fraction = calculateRate(hour, list)

  def calculateRates(list: CountPerSecond): (Double, Double, Double) =
    (ratePerSecond(list).roundedDouble(), ratePerMinute(list).roundedDouble(), ratePerHour(list).roundedDouble())

  private[this] def calculateRate(period: SecondsInPeriod, list: CountPerSecond): Fraction = {

    @tailrec
    def doCalc(list: CountPerSecond, periodLeft: Int, periodSize: Int, f: Fraction, lastValue: Int): Fraction =
      (list, periodLeft) match {
        case (Nil, 0)     => f
        case (Nil, p)     => f.addToNum(p * lastValue)
        case (h :: t, 0)  => doCalc(t, periodSize - 1, periodSize, f.addToNumIncDem(h), h)
        case (h :: t, p)  => doCalc(t, p - 1, periodSize, f.addToNum(h), h)
      }

    doCalc(list, period, period, initFraction, 0)

  }

}

