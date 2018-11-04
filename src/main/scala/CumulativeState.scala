import CumulativeState._
import FractionSyntax._
import com.typesafe.config.ConfigFactory
import TweetSyntax._

case class NameCount(name: String, count: Int)

object CumulativeState {
  type CountMap = Map[String, Int]

  val empty = CumulativeState(0, 0, 0, 0, 0, 0, Map.empty, Map.empty, Map.empty, List.empty)

  val displayCount: Int = ConfigFactory.load.getInt("maxTopDisplay")

  def append(ts: Vector[TwitterObject], state: CumulativeState): CumulativeState = {
    val newState = ts.foldLeft(state)(_ append _)
    newState.copy(countPerSeg = ts.size :: newState.countPerSeg)
  }

  implicit class CountMapOps(map: CountMap) {
    def insert(vs: Seq[String]): CountMap = vs.foldLeft(map){case (m, v) => insertInv(m, v)}
    private def insertInv(m: CountMap, v : String): CountMap = m + ((v, m.withDefaultValue(0)(v) + 1))
    def sortedList(num: Int = displayCount): Seq[NameCount] = map.toSeq.sortWith(_._2 > _._2).take(num).map{case (name,count) => NameCount(name, count)}
  }

}

final case class CumulativeState (
    deleteCount: Int,
    parseErrors: Int,
    tweetCount: Int,
    containedEmojiCount: Int,
    containedUrlCount: Int,
    containsPhotoCount: Int,
    hashtags: CountMap,
    domains: CountMap,
    emojis: CountMap,
    countPerSeg: List[Int]
) {

  def append(t: TwitterObject): CumulativeState = t match {

    case DeleteTweet => this.copy(deleteCount = deleteCount + 1)
    case t: Tweet =>

      val ds = t.domains
      val es = t.emojis

      this.copy(
        tweetCount = tweetCount + 1,
        hashtags = hashtags.insert(t.hashTags),
        containedUrlCount = if (ds.nonEmpty) containedUrlCount + 1 else containedUrlCount,
        domains = domains.insert(ds),
        containedEmojiCount = if (es.nonEmpty) containedEmojiCount + 1 else containedEmojiCount,
        emojis = emojis.insert(es),
        containsPhotoCount = if (t.containPhoto(ds)) containsPhotoCount + 1 else containsPhotoCount
      )

    case ParseError => this.copy(parseErrors = parseErrors + 1)

  }

  def toCurrentStats: CurrentStats = {
    val (ratePerSec, ratePerMin, ratePerHour) = TimeRate.calculateRates(countPerSeg)

    CurrentStats(
      allCount = deleteCount + tweetCount + parseErrors,
      tweetCount = tweetCount,
      deleteCount = deleteCount,
      parseErrors = parseErrors,
      ratePerSecond = ratePerSec,
      ratePerMinute = ratePerMin,
      ratePerHour = ratePerHour,
      percentWithEmojis = Fraction(containedEmojiCount, tweetCount).percentage,
      percentWithUrls = Fraction(containedUrlCount, tweetCount).percentage,
      percentWithPhotos = Fraction(containsPhotoCount, tweetCount).percentage,
      topHashTags = hashtags.sortedList(),
      topEmojis = emojis.sortedList(),
      topDomains = domains.sortedList()
    )
  }

}


final case class CurrentStats(
    allCount: Int,
    tweetCount: Int,
    deleteCount: Int,
    parseErrors: Int,
    ratePerSecond: Double,
    ratePerMinute: Double,
    ratePerHour: Double,
    percentWithEmojis: Double,
    percentWithUrls: Double,
    percentWithPhotos: Double,
    topHashTags: Seq[NameCount],
    topEmojis: Seq[NameCount],
    topDomains: Seq[NameCount]
)
