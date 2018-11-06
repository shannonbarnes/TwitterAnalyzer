

case class Fraction(num: Int, den: Int) {
  def addToNum(v: Int): Fraction = this.copy(num = num + v)
  def addToNumIncDem(v: Int): Fraction = this.copy(num = num + v, den = den + 1)

  def roundedDouble(places: Int = 2, mult: Int = 1): Double =
    if (den == 0) 0 else
    BigDecimal(num.toDouble/den * mult).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble

  def percentage: Double = roundedDouble(mult = 100)

}
