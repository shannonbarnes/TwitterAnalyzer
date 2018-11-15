import State._
import com.typesafe.config.ConfigFactory

object StatsSnapshot {

  val displayCount: Int = ConfigFactory.load.getInt("maxTopDisplay")

  implicit class CountMapOps(map: CountMap) {
    def priorityList(num: Int = displayCount): Seq[NameCount] =
      map.toSeq.sortWith(_._2 > _._2).take(num).map{case (name,count) => NameCount(name, count)}
  }

  def fromState(s: CumulativeState): StatsSnapshot = {
    val all = s.all
    val seconds = if (s.seconds == 0) 1 else s.seconds

    StatsSnapshot(
      allCount = all,
      tweetCount = s.tweetCount,
      deleteCount = s.deleteCount,
      parseErrors = s.parseErrors,
      elapsedSeconds = s.seconds,
      ratePerSecond = Fraction(all, seconds).roundedDouble(),
      ratePerMinute = Fraction(all * 60, seconds).roundedDouble(),
      ratePerHour = Fraction(all * 60 * 60, seconds).roundedDouble(),
      percentWithEmojis = Fraction(s.containedEmojiCount, s.tweetCount).percentage,
      percentWithUrls = Fraction(s.containedUrlCount, s.tweetCount).percentage,
      percentWithPhotos = Fraction(s.containsPhotoCount, s.tweetCount).percentage,
      percentDeletes = Fraction(s.deleteCount, all).percentage,
      topHashTags = s.hashtags.priorityList(),
      topEmojis = s.emojis.priorityList(),
      topDomains = s.domains.priorityList()
    )
  }
}

final case class StatsSnapshot(
    allCount: Int,
    tweetCount: Int,
    deleteCount: Int,
    parseErrors: Int,
    elapsedSeconds: Int,
    ratePerSecond: Double,
    ratePerMinute: Double,
    ratePerHour: Double,
    percentWithEmojis: Double,
    percentWithUrls: Double,
    percentWithPhotos: Double,
    percentDeletes: Double,
    topHashTags: Seq[NameCount],
    topEmojis: Seq[NameCount],
    topDomains: Seq[NameCount]
)


final case class NameCount(name: String, count: Int)

case class Fraction(num: Int, den: Int) {

  def roundedDouble(places: Int = 2, mult: Int = 1): Double =
    if (den == 0) 0 else
      BigDecimal(num.toDouble/den * mult).setScale(places, BigDecimal.RoundingMode.HALF_UP).toDouble

  def percentage: Double = roundedDouble(mult = 100)

}