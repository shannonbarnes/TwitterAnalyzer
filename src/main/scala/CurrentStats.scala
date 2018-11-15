import ConcreteState._

object CurrentStats {

  def fromState(s: CumulativeState): CurrentStats = {
    val all = s.all
    val seconds = if (s.seconds == 0) 1 else s.seconds

    CurrentStats(
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
      topHashTags = s.hashtags.sortedList(),
      topEmojis = s.emojis.sortedList(),
      topDomains = s.domains.sortedList()
    )
  }
}

final case class CurrentStats(
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