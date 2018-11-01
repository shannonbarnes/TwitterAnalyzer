import scala.language.implicitConversions

object FractionSyntax {

  type Percentage = Double

  def round(p: Fraction, mult: Int = 1): Double = {
    if (p.den <= 0) 0 else
    BigDecimal(p.num.toDouble/p.den * mult).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  implicit class FractionPimp(f: Fraction) {
    def percentage: Double = round(f, 100)
    def roundedDouble: Double = round(f)
  }

}

case class Fraction(num: Int, den: Int) {
  def addToNum(v: Int): Fraction = this.copy(num = num + v)
  def addToNumIncDem(v: Int): Fraction = this.copy(num = num + v, den = den + 1)
}
