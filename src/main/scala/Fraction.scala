

case class Fraction(num: Int, den: Int) {

  def roundedDouble(places: Int = 2, mult: Int = 1): Double =
    if (den == 0) 0 else
    BigDecimal(num.toDouble/den * mult).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble

  def percentage: Double = roundedDouble(mult = 100)

}
