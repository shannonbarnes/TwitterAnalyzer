import cats.effect.IO
import fs2.{Chunk, Sink, Stream}
import FractionSyntax._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.ListBuffer

trait DataStoreSink extends Sink[IO, Chunk[TwitterObject]] {

  def apply(s: Stream[IO, Chunk[TwitterObject]]): Stream[IO, Unit] =
    s map {c => insert(c.toVector)}

  def currentStats: CurrentStats
  def insert(ts: Vector[TwitterObject]): Unit

}

object InMemoryDataStore extends DataStoreSink {
  import TweetSyntax._

  def insert(ts: Vector[TwitterObject]): Unit = {
    buffer += ts.size
    ts foreach insertSingle
  }


  def insertSingle(t: TwitterObject): Unit = t match {
    case DeleteTweet => deleteCount = deleteCount + 1
    case t: Tweet =>
      tweetCount += 1
      hashtags.insert(t.hashTags)

      val ts = t.domains
      if (ts.nonEmpty) {
        containedUrlCount += 1
        domains.insert(ts)
      }

      val es = t.emojis
      if (es.nonEmpty) {
        containedEmojiCount += 1
        emojis.insert(es)
      }

      if (t.containPhoto(ts))
        containsPhotoCount += 1

    case ParseError => parseErrors += 1
  }

  val buffer: ListBuffer[Int] = ListBuffer()
  def allCount: Int = deleteCount + tweetCount
  var deleteCount: Int = 0
  var parseErrors: Int = 0
  var tweetCount: Int = 0
  var containedEmojiCount: Int = 0
  var containedUrlCount: Int = 0
  var containsPhotoCount: Int = 0

  val displayCount: Int = ConfigFactory.load.getInt("maxTopDisplay")

  val hashtags = new PriorityList(displayCount)
  val emojis = new PriorityList(displayCount)
  val domains = new PriorityList(displayCount)

  def currentStats: CurrentStats = {
    val (ratePerSec, ratePerMin, ratePerHour) = TimeRate.calculateRates(buffer.toList)

    CurrentStats(
      allCount = allCount,
      tweetCount = tweetCount,
      deleteCount = deleteCount,
      parseErrors = parseErrors,
      ratePerSecond = ratePerSec,
      ratePerMinute = ratePerMin,
      ratePerHour = ratePerHour,
      percentWithEmojis = Fraction(containedEmojiCount, tweetCount).percentage,
      percentWithUrls = Fraction(containedUrlCount, tweetCount).percentage,
      percentWithPhotos = Fraction(containsPhotoCount, tweetCount).percentage,
      topHashTags = hashtags.sortedList,
      topEmojis = emojis.sortedList,
      topDomains = domains.sortedList
    )
  }
}


case class CurrentStats(
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
