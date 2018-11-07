import org.scalatest._
import TimeRate._

class TimeRateSpec extends FlatSpec with Matchers {

  val l1 = List(5)
  val l2 = List(5, 6)
  val l3 = List(5, 6, 7)

  "TimeRate" should "give proper fractions second rates" in {
    ratePerSecond(l1) should be (Fraction(5, 1))
    ratePerSecond(l2) should be (Fraction(5 + 6, 2))
    ratePerSecond(l3) should be (Fraction(5 + 6 + 7, 3))
  }

  it should "give proper fraction minute rates" in {
    ratePerMinute(l1, 5) should be (Fraction(5 * 60, 1))
    ratePerMinute(l2, ratePerSecond(l2).roundedDouble()) should be (Fraction(5 + 6 + (5 + 6)/2d * 58, 1))
    ratePerMinute(l3, ratePerSecond(l3).roundedDouble()) should be (Fraction(5 + 6 + 7 + (5 + 6 + 7)/3d * 57, 1))
  }

  it should "give proper fraction hour rates" in {
    ratePerHour(l1, 5) should be (Fraction(5 * 3600, 1))
    ratePerHour(l2, ratePerSecond(l2).roundedDouble()) should be (Fraction(5 + 6 + (5 + 6)/2d * (3600 - 2), 1))
    ratePerHour(l3, ratePerSecond(l3).roundedDouble()) should be (Fraction(5 + 6 + 7 + (5 + 6 + 7)/3d * (3600 - 3), 1))
  }

  it should "give proper rates for larger lists" in {
    val list = List.fill(60)(2)
    val rpsFraction = ratePerSecond(list)
    val rps = rpsFraction.roundedDouble()

    rpsFraction should be (Fraction(60 * 2, 60))
    ratePerMinute(list, rps) should be (Fraction(60 * 2, 1))
    ratePerHour(list, rps) should be (Fraction(2 * 3600, 1))

    val list2 = List.fill(59)(2)
    ratePerSecond(list2) should be (Fraction(59 * 2, 59))
    val rps2 = ratePerSecond(list2).roundedDouble()
    //These two should be forward filled
    ratePerMinute(list2, rps2) should be (Fraction(60 * 2, 1))
    ratePerHour(list2, rps2) should be (Fraction(2 * 3600, 1))

    val list3 = List.fill(300)(2)
    ratePerSecond(list3) should be (Fraction(300 * 2, 300))
    val rps3 = ratePerSecond(list2).roundedDouble()
    ratePerMinute(list3, rps3) should be (Fraction(300 * 2, 5))
    ratePerHour(list3, rps3) should be (Fraction(2 * 3600, 1))

  }
}