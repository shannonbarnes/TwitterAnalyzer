import FractionSyntax._
import org.scalatest._


class FractionSpec extends FlatSpec with Matchers {

  private def test(in: Double, expectedValue: Double) =  in should be (expectedValue)

  "Fractions" should "convert to a rounded doubles" in {
    test(Fraction(5, 10).percentage, 50.0)
    test(Fraction(1, 3).percentage, 33.33)
    test(Fraction(2, 3).percentage, 66.67)
    test(Fraction(2, 0).percentage, 0.0)

    test(Fraction(5, 10).roundedDouble, .50)
    test(Fraction(1, 3).roundedDouble, .33)
    test(Fraction(2, 3).roundedDouble, .67)
    test(Fraction(2, 0).roundedDouble, 0.0)


    test(Fraction(100, 3).roundedDouble, 33.33)
  }

}