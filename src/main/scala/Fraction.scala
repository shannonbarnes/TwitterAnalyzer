import scala.language.implicitConversions


case class Fraction(num: Int, den: Int) {
  if (den <= 0) throw new IllegalArgumentException("Fraction must have dem > 0")
  def addToNum(v: Int): Fraction = this.copy(num = num + v)
  def addToNumIncDem(v: Int): Fraction = this.copy(num = num + v, den = den + 1)

  def roundedDouble(places: Int = 2, mult: Int = 1): Double =
    BigDecimal(num.toDouble/den * mult).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble

  def percentage: Double = roundedDouble(mult = 100)

}
